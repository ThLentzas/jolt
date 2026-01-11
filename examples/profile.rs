fn main() {
    let input = std::fs::read("examples/data/1MB.json").unwrap();
    let path = "/1018/name";
    let root = jolt::from_slice(&input).unwrap();
    for _ in 0..10000000 {
        root.pointer(path).unwrap();
    }
}