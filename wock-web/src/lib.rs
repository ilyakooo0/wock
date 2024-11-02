#![feature(iter_collect_into)]
mod utils;

use std::rc::Rc;

use dodrio::{
    bumpalo::{self, Bump},
    Attribute, Node, NodeKey, Render, RenderContext,
};
use nock::{
    cue::cue_bytes,
    interpreter::{generate_interpreter_context, slam},
    noun::Noun,
};
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::JsFuture;
use web_sys::Element;

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);

    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

struct WockApp {
    nock: Rc<Noun>,
}

impl<'a> Render<'a> for WockApp {
    fn render(&self, cx: &mut dodrio::RenderContext<'a>) -> dodrio::Node<'a> {
        let mut ctx = generate_interpreter_context();

        let sail = slam(&mut ctx, &self.nock, &Rc::new(Noun::from_u32(0))).unwrap();

        render_sail(cx, &sail)
    }
}

fn render_sail<'a>(ctx: &mut RenderContext<'a>, manx: &Rc<Noun>) -> Node<'a> {
    let (marx, marl) = manx.as_cell().unwrap();
    let (mane, mart) = marx.as_cell().unwrap();

    if mane.is_sig() {
        let contents = mart
            .list_iter()
            .map(|attr| attr.as_cell().unwrap())
            .find(|(key, _)| key.is_sig())
            .map(|(_, value)| tape_to_bump_str(ctx.bump, value))
            .unwrap_or("");
        Node::text(contents)
    } else {
        let mut children = bumpalo::collections::Vec::new_in(&ctx.bump);
        marl.list_iter()
            .map(|child| render_sail(ctx, child))
            .collect_into(&mut children);

        let mut attributes = bumpalo::collections::Vec::new_in(ctx.bump);
        mart.list_iter()
            .map(|attr| {
                let (name, value) = attr.as_cell().unwrap();
                Attribute {
                    name: cord_to_bump_str(ctx.bump, name),
                    value: cord_to_bump_str(ctx.bump, value),
                }
            })
            .collect_into(&mut attributes);

        let listeners = bumpalo::collections::Vec::new_in(ctx.bump);

        Node::element(
            &ctx.bump,
            NodeKey::NONE,
            cord_to_bump_str(ctx.bump, mane),
            listeners.into_bump_slice(),
            attributes.into_bump_slice(),
            children.into_bump_slice(),
            None,
        )
    }
}

fn cord_to_bump_str<'a>(bump: &'a Bump, non: &Rc<Noun>) -> &'a str {
    let bytes =
        bumpalo::collections::Vec::from_iter_in(non.as_bytes().unwrap().iter().cloned(), bump);
    &*bumpalo::collections::String::from_utf8(bytes)
        .unwrap()
        .into_bump_str()
}

fn tape_to_bump_str<'a>(bump: &'a Bump, non: &Rc<Noun>) -> &'a str {
    let bytes = bumpalo::collections::Vec::from_iter_in(
        non.list_iter().map(|char| char.as_u32().unwrap() as u8),
        bump,
    );
    &*bumpalo::collections::String::from_utf8(bytes)
        .unwrap()
        .into_bump_str()
}

#[wasm_bindgen]
pub async fn main(path: &str) {
    let nock = load_nock(path).await;

    let document = gloo::utils::document();
    let output_el: Element = document.get_element_by_id("output").unwrap();

    dodrio::Vdom::new(&output_el, WockApp { nock }).forget();
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
