use xproto::protocol::{xcore as x, present, ExtensionKind};
use xproto::{Request, Message};
use core::convert::TryInto;
use futures::StreamExt;

//type Error = Box<dyn std::error::Error + Send>;
type Error = std::io::Error;

#[tokio::main]
async fn main() -> Result<(), Error> {
    let display = xserver::Display::new(None)?;
    let ((r, w), auth) = xserver::stream::open_display(&display).await?;
    let (mut conn, mut sink) = xserver::stream::XConnection::connect(auth, r, w).await?;
    let setup = conn.setup().clone();
    eprintln!("CONNECTED! {:#?}", setup);

    let events = tokio::spawn(async move {
        while let Some(event) = conn.next().await {
            let event = event?;
            eprintln!("event {:?}", event);
            match event {
                xproto::ExtensionEvent::Core(event) => match event {
                    x::Events::PropertyNotify(x::PropertyNotifyEvent { state, .. }) if matches!(state.get(), x::Property::Delete) => break,
                    _ => (),
                },
                _ => (),
            }
        }

        Ok(())
    });

    let window = sink.generate_id().await?;
    let screen = setup.roots.get(display.screen as usize).unwrap();

    let present = sink.extension(ExtensionKind::Present).await.await?
        .expect("extension not found");
    eprintln!("Present ext {:?}", present);

    let mut create_window = x::CreateWindowRequest {
        length: 0,
        major_opcode: x::CreateWindowRequest::INFO.request_opcode(),
        depth: x::WindowClass::CopyFromParent.into(),
        wid: window,
        parent: screen.root,
        x: 0, y: 0,
        width: 128, height: 128,
        border_width: 0,
        class: x::WindowClass::InputOutput.into(),
        visual: screen.root_visual,
        value_list: x::CreateWindowRequestValueList {
            back_pixel: Some(x::CreateWindowRequestValueListBackPixel {
                background_pixel: screen.black_pixel,
            }),
            event_mask: Some(x::CreateWindowRequestValueListEventMask {
                event_mask: (x::EventMask::VisibilityChange
                    | x::EventMask::PropertyChange
                    | x::EventMask::KeyPress
                    | x::EventMask::KeyRelease
                    | x::EventMask::ButtonPress
                    | x::EventMask::ButtonRelease
                    | x::EventMask::PointerMotion
                    | x::EventMask::ButtonMotion
                    | x::EventMask::StructureNotify
                    | x::EventMask::FocusChange).into(),
            }),
            .. Default::default()
        },
    };
    create_window.length = (create_window.size() / 4).try_into().unwrap();
    sink.execute(create_window).await.await?;

    let event_id = sink.generate_id().await?;
    let mut select_input = present::SelectInputRequest {
        length: 0,
        major_opcode: present.major_opcode,
        minor_opcode: present::SelectInputRequest::INFO.request_opcode(),
        eid: event_id,
        window,
        event_mask: present::EventMask::ConfigureNotify.into(),
    };
    select_input.length = (select_input.size() / 4).try_into().unwrap();
    sink.execute(select_input).await.await?;

    let mut configure_window = x::ConfigureWindowRequest {
        length: 0,
        major_opcode: x::ConfigureWindowRequest::INFO.request_opcode(),
        window,
        value_list: x::ConfigureWindowRequestValueList {
            width: Some(x::ConfigureWindowRequestValueListWidth {
                width: 20,
            }),
            .. Default::default()
        },
    };
    configure_window.length = (configure_window.size() / 4).try_into().unwrap();
    sink.execute(configure_window).await.await?;

    let mut map_window = x::MapWindowRequest {
        window,
        major_opcode: x::MapWindowRequest::INFO.request_opcode(),
        .. Default::default()
    };
    map_window.length = (map_window.size() / 4).try_into().unwrap();
    sink.execute(map_window).await.await?;

    events.await?
}
