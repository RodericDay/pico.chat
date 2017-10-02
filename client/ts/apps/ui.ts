const UserStrings = {
    largeFile: "You are uploading a large file. This may disrupt your connection. Proceed?",
    introMessage: "**Tip**: Address users privately with `@`, and link to other channels with `#`.",
}
let state = {
    title: document.title,
    channel: location.hash,
    username: "",
    users: new Set(),
    messages: [UserStrings.introMessage],
    uploads: [],
    get loggedIn() { return ws && ws.readyState === 1 },
    get streamingOn() { return Object.keys(peerStreams).length > 0 },
}
defaults["chatOn"] = false;
defaults["settingsOn"] = false;
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
    if(user===settings.username&&peerStreams[user]) detectAudio(peerStreams[user]);
}
var viewStream = (user) => {
    let attributes = {
        muted: user === settings.username,
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
                    oninput: (e)=>{settings.username=e.target.value},
                    value: settings.username,
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
    view: () => !settings.chatOn?[]:m("div#chat", [
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
let Settings = {
    view: ()=> !settings.settingsOn?[]:m("div.central-container", [
        m("div#settings", Object.keys(defaults).map(k=>
            [
                m("label", k),
                opts[k]
                ?
                m("select", {
                    onchange: (e) => settings[k] = JSON.parse(e.target.value),
                    value: JSON.stringify(settings[k]),
                },
                [defaults[k], ...opts[k]].map(o => m("option", JSON.stringify(o)))
                )
                :
                m("input", {
                    onchange: (e) => settings[k] = JSON.parse(e.target.value),
                    value: JSON.stringify(settings[k]),
                })
            ]
        )),
    ])
}
let Nav = {
    view: ()=>m("nav.bar", [
        m("img", {src: "svg/channel.svg", onclick: changeChannel, title: "channel"}),
        m("img", {src: "svg/stream.svg", style: {opacity: state.streamingOn?1:0.5}, onclick: ()=>state.streamingOn?streamingStop():streamingStart(), title: "stream"}),
        m("img", {src: "svg/chat.svg", style: {opacity: settings.chatOn?1:0.5}, onclick: ()=>settings.chatOn=!settings.chatOn, title: "chat"}),
        m("img", {src: "svg/settings.svg", style: {opacity: settings.settingsOn?1:0.5}, onclick: ()=>settings.settingsOn=!settings.settingsOn, title: "settings"}),
        m("img", {src: "svg/logout.svg", onclick: logout, title: "log out"}),
    ])
}
let Main = {
    view: ()=>
        state.loggedIn
        ? [m(Chat), m(Streams), m(Settings), m(Nav)]
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
listen("login", (msg) => {
    m.redraw();
});
addEventListener("logout", (e:CustomEvent) => {
    m.redraw();
});
