use std::hint::black_box;
use criterion::{criterion_group, criterion_main, Criterion};

fn bench_parse(c: &mut Criterion) {
    let input = std::fs::read("examples/data/1MB.json").unwrap();
    let root = jolt::from_slice(black_box(&input)).unwrap();
    let path = "/meta/view/columns/5/cachedContents/top/5/count";

    c.bench_function("pointer", |b| {
        b.iter(|| root.pointer(path))
    });
}

criterion_group!(benches, bench_parse);
// this creates the main() we need for this binary
criterion_main!(benches);