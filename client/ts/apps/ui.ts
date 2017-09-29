function onPeerVolume(e:CustomEvent) {
    let user = e.detail.sender;
    let isLoud = e.detail.value > 1000;
    let div = document.querySelector(`.streamContainer.${user} .info`);
    if(div) isLoud ? div.classList.add("loud") : div.classList.remove("loud");
}
function onStream(user) {
    let root = document.querySelector("#streamGrid");
    m.render(root, Object.keys(peerStreams).sort().map(viewStream));
    if(user===state.username&&peerStreams[user]) detectAudio(peerStreams[user]);
}
var viewStream = (user) => {
    let attributes = {
        muted: user === state.username,
        srcObject: peerStreams[user],
    }
    return m(`div.streamContainer.${user}`,
        m("video[controls][autoplay][playsinline]", attributes),
        m("div.info", user),
    )
}
let Streams = {
    // draw this manually for performance reasons
    view: ()=>  m("div#streamGrid", {subtree: "retain"})
}
let Login = {
    view: function() {
        return m("footer", [
            m("form[name=login]", {onsubmit: login}, [
                m("input[name=username]", {
                    oninput: (e)=>{state.username=e.target.value},
                    value: state.username,
                    autocomplete: "off",
                    placeholder: "pick any username!",
                }),
                m("button", {style: "display:none;"}),
            ]),
            m("img", {src: "svg/login.svg", onclick: login, title: "log in"}),
        ])
    }
}
let StatusBar = {
    view: ()=>m("footer", [
        m("img", {src: "svg/stream.svg", style: {opacity: currentConstraints?1:0.5}, onclick: ()=>currentConstraints?streamingStop():streamingStart(), title: "stream"}),
        m("img", {src: "svg/chat.svg", style: {opacity: state.chatOn?1:0.5}, onclick: ()=>state.chatOn=!state.chatOn, title: "chat"}),
        m("img", {src: "svg/logout.svg", onclick: logout, title: "log out"}),
    ])
}
let Main = {
    view: ()=>
        state.loggedIn
        ? [m(StatusBar), m(Chat), m(Streams)]
        : [m(Login)]
}
listen("onStream", onStream);
addEventListener("peerVolume", onPeerVolume);
addEventListener("peerUpdate", (e)=>{m.redraw()});
addEventListener("socketEvent", (e:CustomEvent) => {
    m.redraw();
});
addEventListener("socketError", (e:CustomEvent) => {
    if(e.detail.value) { alert(e.detail.value); }
    m.redraw();
});
addEventListener("login", (e:CustomEvent) => {
    state.status = `logged in as <b>${state.username}</b>`;
    localStorage.username = JSON.stringify(state.username);
    state.loggedIn = true;
    m.redraw();
});
addEventListener("logout", (e:CustomEvent) => {
    state.loggedIn = false;
    m.redraw();
});
