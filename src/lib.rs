#[cfg(feature = "graphviz")]
mod graphviz;

mod algorithms;

use std::{io::Read, fs::File};

use indexmap::{map::Entry, IndexMap};
use once_cell::sync::OnceCell;
use ordered_float::NotNan;
use bitcode::{Encode,Decode};

pub type Cost = NotNan<f64>;

// really stupid design
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NodeId(u32,u32);

#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ClassId(u32);

mod id_impls {
    use super::*;

    impl NodeId {
        pub fn new(n: u32, c: u32) -> Self {
            Self(c,n)
        }

        pub fn class(&self) -> u32 {
            self.0
        }

        pub fn node(&self) -> u32 {
            self.1
        }
    }

    impl<S: Into<String>> From<S> for NodeId {
        fn from(s: S) -> Self {
            let s: String = s.into();
            match s.parse::<u32>() {
                Err(_) => {
                    let split = s.split('.');
                    let split: Vec<&str> = split.collect();
                    if split.len() < 2 {
                        panic!();
                    } else {
                        Self(split[0].parse::<u32>().unwrap(), split[1].parse::<u32>().unwrap())
                    }
                }
                Ok(n) => Self(0,n)
            }
        }
    }

    impl std::fmt::Display for NodeId {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    impl ClassId {
        pub fn new(n: u32) -> Self {
            Self(n)
        }

        pub fn class(&self) -> u32 {
            self.0
        }
    }

    impl<S: Into<String>> From<S> for ClassId {
        fn from(s: S) -> Self {
            let s: String = s.into();
            Self(s.parse::<u32>().unwrap())
        }
    }

    impl std::fmt::Display for ClassId {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }
}

#[derive(Encode,Decode)]
pub struct EnodeBits<'a> {
    pub eclass: u32,
    pub enode: u32,
    pub op: &'a str,
    pub metadata: u8,
    pub children: Vec<u32>
}

#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct EGraph {
    pub nodes: IndexMap<NodeId, Node>,
    #[cfg_attr(feature = "serde", serde(default))]
    pub root_eclasses: Vec<ClassId>,
    // Optional mapping of e-class ids to some additional data about the e-class
    #[cfg_attr(feature = "serde", serde(default))]
    pub class_data: IndexMap<ClassId, ClassData>,
    #[cfg_attr(feature = "serde", serde(skip))]
    once_cell_classes: OnceCell<IndexMap<ClassId, Class>>,
}

fn parse_num(reader: &mut impl Read) -> std::io::Result<u32> {
    let mut buf = [0u8; 1];

    reader.read_exact(&mut buf)?;
    // first byte = number of bytes to follow
    let bytes_len = (buf[0]-1) as usize;

    let mut n: u32 = 0;
    for _ in 0..bytes_len {
        reader.read_exact(&mut buf)?;
        n = (n << 8) + buf[0] as u32;
    }
    Ok(n)
}

impl EGraph {
    /// Adds a new node to the egraph
    ///
    /// Panics if a node with the same id already exists
    pub fn add_node(&mut self, node_id: impl Into<NodeId>, node: Node) {
        match self.nodes.entry(node_id.into()) {
            Entry::Occupied(e) => {
                panic!(
                    "Duplicate node with id {key:?}\nold: {old:?}\nnew: {new:?}",
                    key = e.key(),
                    old = e.get(),
                    new = node
                )
            }
            Entry::Vacant(e) => e.insert(node),
        };
    }

    pub fn nid_to_cid(&self, node_id: &NodeId) -> &ClassId {
        &self[node_id].eclass
    }

    pub fn nid_to_class(&self, node_id: &NodeId) -> &Class {
        &self[&self[node_id].eclass]
    }

    /// Groups the nodes in the e-graph by their e-class
    ///
    /// This is *only done once* and then the result is cached.
    /// Modifications to the e-graph will not be reflected
    /// in later calls to this function.
    pub fn classes(&self) -> &IndexMap<ClassId, Class> {
        self.once_cell_classes.get_or_init(|| {
            let mut classes = IndexMap::new();
            for (node_id, node) in &self.nodes {
                classes
                    .entry(node.eclass.clone())
                    .or_insert_with(|| Class {
                        id: node.eclass.clone(),
                        nodes: vec![],
                    })
                    .nodes
                    .push(node_id.clone())
            }
            classes
        })
    }

    #[cfg(feature = "serde")]
    pub fn from_json_file(path: impl AsRef<std::path::Path>) -> std::io::Result<Self> {
        let file= std::fs::File::open(path)?;
        let egraph: Self = serde_json::from_reader(std::io::BufReader::new(file))?;
        Ok(egraph)
    }

    #[cfg(feature = "serde")]
    pub fn to_json_file(&self, path: impl AsRef<std::path::Path>) -> std::io::Result<()> {
        let file = std::fs::File::create(path)?;
        serde_json::to_writer_pretty(std::io::BufWriter::new(file), self)?;
        Ok(())
    }

    #[cfg(feature = "serde")]
    pub fn test_round_trip(&self) {
        let json = serde_json::to_string_pretty(&self).unwrap();
        let egraph2: EGraph = serde_json::from_str(&json).unwrap();
        assert_eq!(self, &egraph2);
    }

    #[cfg(feature = "serde")]
    pub fn from_binary_file(file: &File) -> std::io::Result<Self> {
        use std::io::{BufReader, Read, Seek};

        let mut egraph = EGraph::default();
        let mut reader = BufReader::new(file);

        // why
        let mut buf_u32 = [0u8; 4];
        reader.read_exact(&mut buf_u32)?;
        let num_root_eclasses = u32::from_le_bytes(buf_u32) as usize;
        for _ in 0..num_root_eclasses {
            reader.read_exact(&mut buf_u32)?;
            let eid = u32::from_le_bytes(buf_u32);
            egraph.root_eclasses.push(eid.to_string().into());
        }
        dbg!(reader.stream_position().unwrap());
        while reader.read_exact(&mut buf_u32[..1]).is_ok() {
            let enodelen: u32 = if buf_u32[0] & 0x80 != 0 {
                reader.read_exact(&mut buf_u32[1..])?;
                let n = u32::from_le_bytes(buf_u32);
                (n & 0x7F) + ((n & 0xFFFFFF) >> 1)
            } else {
                buf_u32[0] as u32
            };
            let mut enode_bytes: Vec<u8> = vec![0; enodelen as usize];
            reader.read_exact(&mut enode_bytes.as_mut_slice())?;
            let enode_ser: EnodeBits<'_> = bitcode::decode(enode_bytes.as_slice()).unwrap();
            let children: Vec<NodeId> = enode_ser.children.iter().map(|c| NodeId::from(format!("{}.0", c))).collect();
            let enodeid = NodeId::new(enode_ser.enode, enode_ser.eclass);
            let eclassid = ClassId::new(enode_ser.eclass);
            let enode = Node {
                children,
                op: enode_ser.op.to_string(),
                eclass: eclassid,
                cost: if (enode_ser.metadata & 0x40) == 0 {0.0} else {1.0}.try_into().unwrap(),
                subsumed: (enode_ser.metadata & 0x20) != 0
            };
            egraph.nodes.insert(enodeid, enode);
        }  
        Ok(egraph)
        
    }
}

impl std::ops::Index<&NodeId> for EGraph {
    type Output = Node;

    fn index(&self, index: &NodeId) -> &Self::Output {
        self.nodes
            .get(index)
            .unwrap_or_else(|| panic!("No node with id {:?}", index))
    }
}

impl std::ops::Index<&ClassId> for EGraph {
    type Output = Class;

    fn index(&self, index: &ClassId) -> &Self::Output {
        self.classes()
            .get(index)
            .unwrap_or_else(|| panic!("No class with id {:?}", index))
    }
}

#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Node {
    pub op: String,
    #[cfg_attr(feature = "serde", serde(default))]
    pub children: Vec<NodeId>,
    pub eclass: ClassId,
    #[cfg_attr(feature = "serde", serde(default = "one"))]
    pub cost: Cost,
    #[cfg_attr(feature = "serde", serde(default))]
    pub subsumed: bool,
}

impl Node {
    pub fn is_leaf(&self) -> bool {
        self.children.is_empty()
    }
}

fn one() -> Cost {
    Cost::new(1.0).unwrap()
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Class {
    pub id: ClassId,
    pub nodes: Vec<NodeId>,
}

#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassData {
    #[cfg_attr(feature = "serde", serde(rename = "type"))]
    pub typ: Option<String>,
}
