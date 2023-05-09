use std::{ffi::CString, thread};

use gtk4::{
    glib::{
        once_cell::sync::OnceCell, MainContext, PropertyGet, SendWeakRef, Sender, PRIORITY_DEFAULT,
    },
    prelude::{ApplicationExt, ApplicationExtManual, Continue},
    traits::GtkWindowExt,
    Application, ApplicationWindow,
};
use hs_bindgen::*;

static CHANNEL: OnceCell<Sender<String>> = OnceCell::new();

#[hs_bindgen(run::IO())]
fn run_app() {
    thread::spawn(|| {
        let app = Application::builder()
            .application_id("org.shekhinah.meww")
            .build();

        let _guard = app.hold();

        app.connect_activate(|app| {
            let send_ref = SendWeakRef::new();
            send_ref.set(Some(app));
            let (sender, receiver) = MainContext::channel(PRIORITY_DEFAULT);

            receiver.attach(None, move |msg: String| {
                send_ref.get(|app| {
                    app.as_ref().map(|app| {
                        let window = ApplicationWindow::builder()
                            .application(app)
                            .title(&msg)
                            .build();

                        gtk4_layer_shell::init_for_window(&window);

                        // Display above normal windows
                        gtk4_layer_shell::set_layer(&window, gtk4_layer_shell::Layer::Overlay);

                        // Push other windows out of the way
                        gtk4_layer_shell::auto_exclusive_zone_enable(&window);

                        let anchors = [
                            (gtk4_layer_shell::Edge::Left, true),
                            (gtk4_layer_shell::Edge::Right, true),
                            (gtk4_layer_shell::Edge::Top, true),
                            (gtk4_layer_shell::Edge::Bottom, false),
                        ];

                        let label = gtk4::Label::new(Some(""));
                        label
                            .set_markup("<span font_desc=\"20.0\">GTK Layer Shell example!</span>");
                        window.set_child(Some(&label));

                        for (anchor, state) in anchors {
                            gtk4_layer_shell::set_anchor(&window, anchor, state);
                        }

                        window.present();
                    })
                });

                Continue(true)
            });

            CHANNEL.set(sender).unwrap();
        });

        app.run();
    });
}

#[hs_bindgen(printNum :: FunPtr (IO CInt) -> IO ())]
fn print(v: Box<dyn Fn() -> i32>) {
    println!("{}", v());
}

#[hs_bindgen(createWindow :: CString -> IO ())]
fn create_window(name: CString) {
    CHANNEL.wait().send(name.into_string().unwrap()).unwrap();
}
