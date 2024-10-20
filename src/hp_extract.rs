use crate::objtypes::*;
use anyhow::{anyhow, Result};
use console::style;
use nom::bytes::complete::take;
use nom::combinator::map;
use nom::multi::{count, length_count};
use nom::IResult;
use std::f32::consts::E;
use std::fmt;
use std::path::{Path, PathBuf};

fn dump_nibbles(nibs: &[u8]) {
    let l = std::cmp::min(nibs.len(), 200);
    // print hex dump of the nibs, with 16 nibbles per line, as well as with ascii representation
    for i in (0..l).step_by(16) {
        let mut line = String::new();
        for j in i..i + 16 {
            if j < nibs.len() {
                line.push_str(&format!("{:01x} ", nibs[j]));
            } else {
                line.push_str("  ");
            }
        }
        line.push_str("  ");
        // gather the nibs 2 by 2 to get the ascii representation
        for j in 0..8 {
            if i + j * 2 + 1 < nibs.len() {
                let mut b = nibs[i + j * 2 + 1];
                b <<= 4;
                b |= nibs[i + j * 2];
                let c = b as char;
                if c.is_ascii_alphanumeric() {
                    line.push(c);
                } else {
                    line.push('.');
                }
            } else {
                line.push(' ');
            }
        }
        line.push_str("  ");
        for j in 0..8 {
            if i + 2 + j * 2 < nibs.len() {
                let mut b = nibs[i + 2 + j * 2];
                b <<= 4;
                b |= nibs[i + 1 + j * 2];
                let c = b as char;
                if c.is_ascii_alphanumeric() {
                    line.push(c);
                } else {
                    line.push('.');
                }
            } else {
                line.push(' ');
            }
        }
        println!("{}", line);
    }
}
fn to_nibble_array(data: &[u8]) -> Vec<u8> {
    let mut nibble_array = Vec::new();
    for byte in data {
        nibble_array.push(byte & 0x0f);
        nibble_array.push(byte >> 4);
    }
    nibble_array
}

fn from_nibbles(nibbles: &[u8]) -> u32 {
    let mut result = 0;
    for nibble in nibbles {
        result = (result << 4) | (*nibble as u32);
    }
    result
}

fn from_nibbles_u16(nibbles: &[u8]) -> u16 {
    let mut result = 0;
    for nibble in nibbles {
        result = (result << 4) | (*nibble as u16);
    }
    result
}
// bcd encoded integer. last nibble is the sign unless everything is 0
fn bcd_from_nibbles(nibbles: &[u8]) -> i128 {
    let mut result = 0;
    if nibbles.len() <= 1 {
        return 0;
    }
    for i in 0..nibbles.len() - 1 {
        result = (result * 10) + (nibbles[i] as i128);
    }
    if nibbles[nibbles.len() - 1] != 0 {
        result = -result;
    }
    result
}

// Parsers for various object types
fn parse_real(input: &[u8]) -> IResult<&[u8], Real> {
    let (input, exp) = map(take(3usize), from_nibbles_u16)(input)?;
    let (input, mantissa) = map(take(12usize), from_nibbles)(input)?;
    let (input, sign) = map(take(1usize), |x:&[u8]| x[0])(input)?;
    Ok((
        input,
        Real {
            exponent: exp,
            mantissa,
            sign,
        },
    ))
}
macro_rules! take_integer {
    ($name:ident, $count:expr) => {
        fn $name(input: &[u8]) -> IResult<&[u8], u32> {
            let (input, nibs) = take($count)(input)?;

            let mut addr = 0u32;
            for i in (0..$count).rev() {
                addr <<= 4;
                addr |= nibs[i] as u32;
            }
            Ok((input, addr))
        }
    };
}

take_integer!(take_integer5, 5usize);
take_integer!(take_integer3, 3usize);
take_integer!(take_integer2, 2usize);
fn take_ascii(len: u32, input: &[u8]) -> IResult<&[u8], String> {
    let (input, nibs) = take(len * 2)(input)?;
    let mut s = String::new();
    for i in 0..len {
        let mut b = nibs[i as usize * 2 + 1];
        b <<= 4;
        b |= nibs[i as usize * 2];
        s.push(b as char);
    }
    Ok((input, s))
}
fn extract_semi_terminated(nibs: &[u8]) -> IResult<&[u8], Vec<Obj>> {
    let mut objs = Vec::new();
    let mut nibs = nibs;
    loop {
        let (_nibs, obj) = extract_obj(nibs)?;
        nibs = _nibs;
        match obj {
            Obj::Semi() => {
                break;
            }
            _ => {}
        }
        objs.push(obj);
    }
    Ok((nibs, objs))
}

#[derive(Debug)]
struct DirEntity {
    name: String,
    obj: Obj,
}
#[derive(Debug)]
struct Dir {
    entities: Vec<DirEntity>,
}
fn extract_dir_entity(nibs: &[u8]) -> IResult<&[u8], DirEntity> {
    // An entity consists of an ASCIX name followed by the contents of
    // the object. We need to read the ASCIX name, then the object
    // contents.
    let (nibs, name_len) = take_integer2(nibs)?;
    let (nibs, name) = take_ascii(name_len, nibs)?;
    let (nibs, name_len_back) = take_integer2(nibs)?;
    assert_eq!(name_len, name_len_back);
    println!(
        "name: {} len: {} unknown: {}",
        name, name_len, name_len_back
    );
    let (nibs, obj) = extract_obj(nibs)?;
    println!("extracted object: {:?}", obj);
    dump_nibbles(&nibs);

    Ok((
        nibs,
        DirEntity {
            name: name,
            obj: obj,
        },
    ))
}
// Prologue (5 nibbles): 02A96 – This identifies the object type as a subdirectory.

// Number of Attached Libraries (3 nibbles): This field indicates how many libraries are currently attached to this specific subdirectory. Only one library can be directly associated with a subdirectory; HOME has more flexible library attachment. A value of 7FFh (hex) signifies that no libraries are attached.

// Offset to Last Object (5 nibbles): This is a crucial pointer. It represents the memory offset (in nibbles) to the last object within the subdirectory. It helps the calculator quickly navigate to the end of the directory's contents when searching.

// Five Null Nibbles (5 nibbles): These are always present and probably act as padding or a marker to separate the header information from the actual object entries.

// Object Entries (variable): This is the main part. Each object entry consists of the following:

// Number of Characters in Object Name (2 nibbles): Length of the object's name (ASCII).
// Object Name (variable): The object's name. ASCII encoding is used; each character takes two nibbles.
// Object Data (variable): This is the object itself. It can be any type of HP 48/49 object. The size and type of this object vary.
// Size of Object Data (5 nibbles): The size of the full object data (including name and lenght) (in nibbles) follows.

fn extract_dir(nibs: &[u8]) -> IResult<&[u8], Dir> {

    let (nibs, attached_libs) = take_integer3(nibs)?;
    let (nibs, offset) = take_integer5(nibs)?;
    let (nibs, _zeros) = take_integer5(nibs)?;
    let offset = offset - 10;
    println!("attached_libs: {:x}, offset: {:x}", attached_libs, offset);
    let mut entities = Vec::new();
    // offset is the offset of the last object of the directory
    let last_obj_slice = &nibs[offset as usize..];
    let mut nibs = nibs;
    dump_nibbles(nibs);
    loop {
        let (nibs_, entity) = extract_dir_entity(nibs)?;
        entities.push(entity);
        if nibs.as_ptr() >= last_obj_slice.as_ptr() {
            nibs = nibs_;
            break;
        }
        // 5 nibbles after each object but the last one (so that the dir can be parsed backwards)
        let (nibs_, _) = take_integer5(nibs_)?;
        nibs = nibs_;
    }
    Ok((nibs, Dir { entities: entities }))
}
// Data structures
#[derive(Debug)]
pub struct Real {
    exponent: u16,
    mantissa: u32,
    sign: u8,
}

#[derive(Debug)]
pub struct Complex {
    real: Real,
    imag: Real,
}

// *** Enhanced Object Parsers ***
fn parse_complex(input: &[u8]) -> IResult<&[u8], Complex> {
    let (input, real) = parse_real(input)?;
    let (input, imag) = parse_real(input)?;
    Ok((input, Complex { real, imag }))
}
#[derive(Debug)]
pub struct Array {
    obj_type: u32,
    num_dims: u32,
    dims: Vec<u32>,
    objects: Vec<Obj>,
}


fn parse_array(input: &[u8]) -> IResult<&[u8], Array> {
    let (input, size) = map(take(5usize), from_nibbles)(input)?;
    let (input, obj_type) = map(take(5usize), from_nibbles)(input)?;
    let (input, num_dims) = map(take(5usize), from_nibbles)(input)?;
    let (input, dims) = count(map(take(5usize), from_nibbles), num_dims as usize)(input)?;
    let num_objs = dims.iter().fold(1, |acc, x| acc * x);

    let (input, objects) = count(|x|extract_obj_with_prolog(x, obj_type), num_objs as usize)(input)?;
    Ok((input, Array{obj_type, num_dims, dims, objects}))
}

fn parse_integer(input: &[u8]) -> IResult<&[u8], i32> {
    let (input, size) = take_integer5(input)?;
    let (input, digits) = map(take((size-5) as usize), bcd_from_nibbles)(input)?;
    Ok((input, digits as i32))
}


#[derive(Debug)]
pub enum Obj {
    Dir(Dir),
    Real(Real),
    Int(i32),
    CStr(String),
    Prg(Vec<Obj>),
    List(Vec<Obj>),
    Symb(Vec<Obj>),
    Unit(Vec<Obj>),
    Complex(Complex),
    Array(Array),
    Integer(i128), 
    Ext(u32),
    ExtObj(u32, Vec<u8>),
    FixedObj(u32, Vec<u8>),
    Code(Vec<u8>),
    GlobalName(String),
    LocalName(String),
    Tagged(String),
    Semi(),
}

fn extract_obj(nibs: &[u8]) -> IResult<&[u8], Obj> {
    let (nibs, prolog) = take_integer5(nibs)?;
    extract_obj_with_prolog(nibs, prolog)
}

fn extract_obj_with_prolog(nibs: &[u8], prolog: u32 ) -> IResult<&[u8], Obj> {
    println!("prolog: {:x}", prolog);
    dump_nibbles(nibs);
    match prolog {
        DORRP => {
            //Dir
            let (nibs, d) = extract_dir(nibs)?;
            return Ok((nibs, Obj::Dir(d)));
        }
        DOREAL => map(parse_real, Obj::Real)(nibs),

        DOFLASHP | DOBINT  | DOEREAL | DOCMP | DOECMP | DOCHAR | DOROMP => {
            let size: usize = match prolog {
                DOFLASHP => 7,
                DOBINT => 5,
                DOREAL => 16,
                DOEREAL => 21,
                DOCMP => 32,
                DOECMP => 42,
                DOCHAR => 2,
                DOROMP => 6,
                _ => unreachable!(),
            };
            let (nibs, data) = take(size)(nibs)?;
            return Ok((nibs, Obj::FixedObj(prolog, data.to_vec())));
        }
        DOINT => map(parse_integer, |x| Obj::Int(x))(nibs),
        DOCSTR => {
            let (nibs, sz) = take_integer5(nibs)?;
            let sz = sz - 5;
            let (nibs, cstr) = take(sz)(nibs)?;
            let (_, cstr) = take_ascii(sz / 2, cstr)?;
            return Ok((nibs, Obj::CStr(cstr)));
        }
        DOCOL | DOLIST | DOSYMB | DOEXT => {
            let (nibs, objs) = extract_semi_terminated(nibs)?;
            let obj = match prolog {
                DOCOL => Obj::Prg(objs),
                DOLIST => Obj::List(objs),
                DOSYMB => Obj::Symb(objs),
                DOEXT => Obj::Unit(objs),
                _ => unreachable!(),
            };
            return Ok((nibs, obj));
        }
        DOCODE => {
            let (nibs, sz) = take_integer5(nibs)?;
            let sz = sz - 5;
            let (nibs, code) = take(sz)(nibs)?;
            return Ok((nibs, Obj::Code(code.to_vec())));
        }
        SEMI => {
            return Ok((nibs, Obj::Semi()));
        }
        DOEXT1 | DOEXT2 | DOEXT3 | DOEXT4 | DOGROB | DOARRY | DOLNKARRY | DOHSTR | DOLIB
        | DOBAK | DOEXT0 => {
            let (nibs, sz) = take_integer5(nibs)?;
            let sz = sz - 5;
            let (nibs, data) = take(sz)(nibs)?;
            return Ok((nibs, Obj::ExtObj(prolog, data.to_vec())));
        }
        DOIDNT | DOLAM | DOTAG => {
            let (nibs, sz) = take_integer5(nibs)?;
            let sz = sz - 5;
            let (nibs, data) = take_ascii(sz, nibs)?;
            return Ok((nibs, match prolog {
                DOIDNT => Obj::GlobalName(data),
                DOLAM => Obj::LocalName(data),
                DOTAG => Obj::Tagged(data),
                _ => unreachable!(),
            }));
        }
        0..0x1000 => {
            return Err(nom::Err::Error(nom::error::Error::new(
                nibs,
                nom::error::ErrorKind::Tag,
            )));
        }
        _ => {
            return Ok((nibs, Obj::Ext(prolog)));
        }
    }
}

pub(crate) fn extract_objs(path: &Path) -> Result<()> {
    // read the file
    let file_contents = std::fs::read(path)?;
    let romrev_header = &file_contents[0..6];
    if romrev_header != b"HPHP48" && romrev_header != b"HPHP49" {
        return Err(anyhow::anyhow!(
            "file is not an HP 48 binary object (does not start with HPHP48)"
        ));
    }
    let mut nibble_array = to_nibble_array(&file_contents[8..]);
    nibble_array.extend_from_slice([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0].as_ref());
    dump_nibbles(&nibble_array);
    match extract_obj(&nibble_array) {
        Ok((_, obj)) => {
            println!("extracted object: {:?}", obj);
        }
        Err(e) => {
            println!("error extracting object: {:?}", e);
        }
    }
    Ok(())
}
