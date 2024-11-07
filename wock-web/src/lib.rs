#![feature(iter_collect_into)]
mod utils;

use core::panic;
use std::{cell::RefCell, rc::Rc};

use dodrio::{
    bumpalo::{self, Bump},
    Attribute, Listener, Node, NodeKey, Render, RenderContext, RootRender, VdomWeak,
};
use nock::{
    cue::cue_bytes,
    interpreter::{generate_interpreter_context, slam, InterpreterContext},
    noun::{cell, Noun},
    wash,
};
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::{spawn_local, JsFuture};
use web_sys::{console::error_1, Element};

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);

    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);

    #[wasm_bindgen(js_namespace = console)]
    fn error(s: &str);
}

#[derive(Clone)]
struct WockApp {
    nock: Rc<Noun>,
    ctx: RefCell<InterpreterContext>,
    model: Rc<Noun>,
    urbit: RefCell<Option<Rc<Noun>>>,
}

impl WockApp {
    async fn get_urbit(self: &Self) -> Rc<Noun> {
        match self.urbit.borrow().clone() {
            Some(urbit) => urbit,
            None => {
                let urbit = load_nock("/urbit.nock").await.unwrap_or_else(|err| {
                    error_1(&err);
                    panic!("Could not load urbit.")
                });

                self.urbit.replace(Some(urbit.clone()));

                urbit
            }
        }
    }
}

impl<'a> Render<'a> for WockApp {
    fn render(&self, cx: &mut dodrio::RenderContext<'a>) -> dodrio::Node<'a> {
        match slam(
            &mut *self.ctx.borrow_mut(),
            &self.nock,
            &cell(&Rc::new(Noun::from_bytes(b"view")), &self.model),
        ) {
            Ok(sail) => render_sail(cx, sail),
            Err(tanks) => {
                let wock_app = self.clone();
                let mut ctx = (*self.ctx.borrow()).clone();
                spawn_local(async move {
                    let urbit = wock_app.get_urbit().await;
                    let str = wash(&mut ctx, &urbit, tanks);
                    error_1(&JsValue::from_str(&str));
                });
                panic!("Could not render");
            }
        }
    }
}

fn render_sail<'a>(ctx: &mut RenderContext<'a>, manx: Rc<Noun>) -> Node<'a> {
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
            .map(|child| render_sail(ctx, child.clone()))
            .collect_into(&mut children);

        let mut attributes = bumpalo::collections::Vec::new_in(ctx.bump);
        let mut listeners = bumpalo::collections::Vec::new_in(ctx.bump);

        mart.list_iter().for_each(|attr| {
            let (name, value) = attr.as_cell().unwrap();
            match &*name.as_bytes().unwrap() {
                b"on-click" => listeners.push(Listener {
                    event: "click",
                    callback: {
                        let value = value.clone();
                        ctx.bump.alloc(
                            move |root: &mut dyn RootRender,
                                  vdom: VdomWeak,
                                  _event: web_sys::Event| {
                                let app = root.unwrap_mut::<WockApp>();

                                let mut ctx = app.ctx.borrow_mut();

                                let sig = ctx.nouns.sig.clone();

                                let event = cell(&value, &sig);

                                match slam(
                                    &mut ctx,
                                    &app.nock,
                                    &cell(
                                        &Rc::new(Noun::from_bytes(b"move")),
                                        &cell(&app.model, &event),
                                    ),
                                ) {
                                    Ok(new_model) => {
                                        app.model = new_model;
                                        vdom.schedule_render();
                                    }
                                    Err(_) => error(&*format!(
                                        "Application failed to process event: {}",
                                        event
                                    )),
                                };
                            },
                        )
                    },
                }),
                _ => attributes.push(Attribute {
                    name: cord_to_bump_str(ctx.bump, name),
                    value: tape_to_bump_str(ctx.bump, value),
                }),
            };
        });

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
    let nock = load_nock(path).await.unwrap_or_else(|err| {
        error_1(&err);
        panic!("Could not load nock.")
    });

    let document = gloo::utils::document();
    let output_el: Element = document.get_element_by_id("output").unwrap();

    let mut ctx = generate_interpreter_context();

    let model = slam(
        &mut ctx,
        &nock,
        &cell(&Rc::new(Noun::from_bytes(b"init")), &Rc::new(Noun::SIG)),
    );
    let model = match model {
        Ok(model) => model,
        Err(tanks) => {
            let urbit = load_nock("/urbit.nock").await.unwrap_or_else(|err| {
                error_1(&err);
                panic!("Could not load urbit.")
            });

            let str = wash(&mut ctx, &urbit, tanks);
            error_1(&JsValue::from_str(&str));
            panic!("Could not load initial model.");
        }
    };

    dodrio::Vdom::new(
        &output_el,
        WockApp {
            nock,
            model,
            ctx: RefCell::new(ctx),
            urbit: RefCell::new(None),
        },
    )
    .forget();
}

pub async fn load_nock(path: &str) -> Result<Rc<Noun>, JsValue> {
    let window = web_sys::window().unwrap();

    let resp = JsFuture::from(window.fetch_with_str(path)).await?;
    assert!(resp.is_instance_of::<web_sys::Response>());
    let resp: web_sys::Response = resp.dyn_into()?;
    let blob = JsFuture::from(resp.blob()?).await?;
    assert!(blob.is_instance_of::<web_sys::Blob>());
    let blob: web_sys::Blob = blob.dyn_into()?;

    let nock = js_sys::Uint8Array::new(&JsFuture::from(blob.array_buffer()).await?).to_vec();

    let nock = cue_bytes(&*nock);
    let (_typ, nok) = nock.as_cell().unwrap();

    Ok(nok.clone())
}
