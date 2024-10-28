mod utils;

use std::rc::Rc;

use gloo::events::EventListener;
use nock::{
    cue::cue_bytes,
    interpreter::{generate_interpreter_context, slam},
    noun::Noun,
};
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::JsFuture;
use web_sys::{Element, HtmlInputElement};

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

static mut LISTENER: Option<EventListener> = None;

#[wasm_bindgen]
pub async fn main(path: &str) {
    let nock = load_nock(path).await;

    let mut ctx = generate_interpreter_context();

    let document = gloo::utils::document();
    let input_element: web_sys::HtmlInputElement = document
        .get_element_by_id("input")
        .unwrap()
        .dyn_into()
        .unwrap();
    let output_el: Element = document.get_element_by_id("output").unwrap();
    unsafe {
        LISTENER = Some(EventListener::new(&input_element, "input", move |ev| {
            let el: HtmlInputElement = ev.target().unwrap().dyn_into().unwrap();

            let bar = slam(
                &mut ctx,
                &nock,
                &Rc::new(Noun::from_bytes(el.value().as_bytes())),
            )
            .unwrap();

            let foo = bar.as_bytes().unwrap();
            let qux = String::from_utf8(foo).unwrap();

            output_el.set_inner_html(&*qux);
        }));
    }

    // window.get_eleme
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
