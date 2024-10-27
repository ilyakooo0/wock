mod utils;

use std::rc::Rc;

use nock::{
    cue::cue_bytes,
    interpreter::{generate_interpreter_context, slam},
    noun::Noun,
};
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::JsFuture;

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);

    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

#[wasm_bindgen]
pub fn greet() {
    alert("Hello, wock-web!");
}

#[wasm_bindgen]
pub async fn main(path: &str) {
    let nock = load_nock(path).await;

    let mut ctx = generate_interpreter_context();
    let bar = slam(&mut ctx, &nock, &Rc::new(Noun::from_bytes(b"Curtis"))).unwrap();

    let foo = bar.as_bytes().unwrap();
    let qux = String::from_utf8(foo).unwrap();

    log(&*qux);
}

pub async fn load_nock(path: &str) -> Rc<Noun> {
    let window = web_sys::window().unwrap();

    let resp = JsFuture::from(window.fetch_with_str(path)).await.unwrap();
    assert!(resp.is_instance_of::<web_sys::Response>());
    let resp: web_sys::Response = resp.dyn_into().unwrap();
    let blob = JsFuture::from(resp.blob().unwrap()).await.unwrap();
    assert!(blob.is_instance_of::<web_sys::Blob>());
    let blob: web_sys::Blob = blob.dyn_into().unwrap();

    let nock =
        js_sys::Uint8Array::new(&JsFuture::from(blob.array_buffer()).await.unwrap()).to_vec();

    let nock = cue_bytes(&*nock);
    let (_typ, nok) = nock.as_cell().unwrap();

    nok.clone()
}
