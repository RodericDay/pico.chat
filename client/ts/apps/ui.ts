function changeChannel() {
    let current = state.channel || "lobby";
    let ans = prompt(`You are in the channel "${current}". Where do you want to go?`);
    if(ans===null) return
    location.hash = ans;
    location.reload();
}
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
        return m("#splash", [
            m("form[name=login].bar", {onsubmit: login}, [
                m("input[name=username]", {
                    oninput: (e)=>{state.username=e.target.value},
                    value: state.username,
                    autocomplete: "off",
                    placeholder: "pick any username!",
                }),
                m("button", {style: "display:none;"}),
                m("img", {src: "svg/login.svg", onclick: login, title: "log in"}),
            ]),
        ])
    }
}
let Chat = {
    view: () => !state.chatOn?[]:m("div#chat", [
        m("div#chat-log", state.messages.map(renderPost)),
        m("form#chat-form", {onsubmit: post},
          m("img", {onclick: clear, src: "svg/clear.svg", title: "clear log"}),
          m("input[name=text]", {autocomplete: "off"}),
          m("img", {onclick: post, src: "svg/post.svg", title: "post message"}),
          m("img", {src: "svg/upload.svg", onclick: upload, title: "upload file"}),
        ),
        m(Upload),
        m("details#chat-userlist",
            m("summary#chat-usercount", `${state.channel||"lobby"} (${state.users.size} online)`),
            m("div", sorted(state.users).join(', ')),
        ),
    ])
}
let Nav = {
    view: ()=>m("nav.bar", [
        m("img", {src: "svg/channel.svg", onclick: changeChannel, title: "channel"}),
        m("img", {src: "svg/stream.svg", style: {opacity: currentConstraints?1:0.5}, onclick: ()=>currentConstraints?streamingStop():streamingStart(), title: "stream"}),
        m("img", {src: "svg/chat.svg", style: {opacity: state.chatOn?1:0.5}, onclick: ()=>state.chatOn=!state.chatOn, title: "chat"}),
        m("img", {src: "svg/logout.svg", onclick: logout, title: "log out"}),
    ])
}
let Main = {
    view: ()=>
        state.loggedIn
        ? [m(Nav), m(Chat), m(Streams)]
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
