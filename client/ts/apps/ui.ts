defaults["chatOn"] = true;
defaults["settingsOn"] = false;
const UserStrings = {
    largeFile: "You are uploading a large file. This may disrupt your connection. Proceed?",
    introMessage:
        "**Tip**: Address users privately with `@`, and link to other channels with `#`. Made by [roderic](https://roderic.ca).",
}
const hiddenSettings = new Set(["username", "channel", "settingsOn", "chatOn"]);
let state = {
    title: document.title,
    users: new Set(),
    messages: [UserStrings.introMessage],
    uploads: [],
    busy: false,
    get loggedIn() { return ws && ws.readyState === 1 && state.users.size > 0 },
    get streamingOn() { return Object.keys(peerStreams).length > 0 },
}
function login(event) {
    event.preventDefault();
    openConnection(settings.username, settings.channel);
}
function logout() {
    state.users.clear();
    ws.close();
}
function changeChannel() {
    let current = settings.channel || "lobby";
    let ans = prompt(`You are in the channel "${current}". Where do you want to go?`);
    if(ans===null) return
    location.hash = ans;
    location.reload();
}
function onPeerVolume(e:CustomEvent) {
    const user = e.detail.sender;
    const isLoud = e.detail.value > 1000;
    const div = document.querySelector(`.streamContainer.${user} .info`);
    if(div) isLoud ? div.classList.add("loud") : div.classList.remove("loud");
}
function onStream(user) {
    renderStreams();
    if(user===settings.username&&peerStreams[user]) detectAudio(peerStreams[user]);
}
function renderStreams() {
    const root = document.querySelector("#streamGrid");
    m.render(root, Object.keys(peerStreams).sort().map(viewStream));
}
var viewStream = (user) => {
    const stream = peerStreams[user];
    const attributes = {
        style: {transform: "scaleX(-1)"},
        playsinline: true,
        autoplay: true,
        controls: settings.controls,
        muted: user === settings.username,
        srcObject: stream,
    }
    const vnode = <any>m("video", attributes);
    const flip = () => vnode.dom.style.transform = vnode.dom.style.transform ? "" : "scaleX(-1)";
    return m(`div.streamContainer.${user}`, {key: user},
        vnode,
        m("div.info", user, [
            m("button.flip", {onclick: flip}, "flip"),
        ]),
    )
}
const Streams = {
    // draw this manually for performance reasons
    view: ()=>m("div#streamGrid", {subtree: "retain"})
}
const Upload = {
    view: ()=>[
        m("input#fileInput[type=file][multiple][hidden]", {onchange: uploadFile}),
        state.uploads.map(([name, url, size], i)=>[
                m("a", {download: name, href: url}, `${name} (${size})`),
            ]
        ),
   ]
}
const Chat = {
    oncreate: function(vnode) {
        scrollToNewest();
        growHeight(vnode);
    },
    onbeforeremove: shrinkHeight,
    view: () => m("div#chat", [
        m("div#chat-log", state.messages.map(renderPost)),
        m("div#chat-bar", [
            m("button.clear", {onclick: clear}, "clear"),
            m("textarea#chat-input", {onkeydown: hotkey, autocomplete: "off"}),
            m("button.post", {onclick: post}, "post"),
            m("button.upload", {onclick: upload}, "upload"),
        ]),
        m(Upload),
    ])
}
const Settings = {
    hide(e) {settings.settingsOn=false},
    keep(e) {e.stopPropagation()},
    listOptions(k) {
        return [defaults[k], ...opts[k]].map(o => m("option", JSON.stringify(o)))
    },
    visibleFields() {
        return Object.keys(defaults).filter(k=>!hiddenSettings.has(k))
    },
    renderField(k) {
        const attrs = {
            value: JSON.stringify(settings[k]),
            onchange(e){
                settings[k] = JSON.parse(e.target.value);
                renderStreams();
            },
        }
        return opts[k]
            ? m("select", attrs, this.listOptions(k))
            : m("input", attrs)
    },
    view() {
        return m("div.centered-overlay", {onclick: this.hide},
            m("table#settings", {onclick: this.keep}, this.visibleFields().map(k=>
                m("tr", [
                    m("td", k),
                    m("td", this.renderField(k)),
                ]),
            )),
        )
    }
}
const Actions = {
    oncreate: growWidth,
    onbeforeremove: shrinkWidth,
    view: () => m("div", [
        m("button.settings", {style: {opacity: settings.settingsOn?1:0.5}, onclick: ()=>settings.settingsOn=!settings.settingsOn}, "settings"),
        m("button.chat", {style: {opacity: settings.chatOn?1:0.5}, onclick: ()=>settings.chatOn=!settings.chatOn}, "chat"),
        m("button.stream", {style: {opacity: state.streamingOn?1:0.5}, onclick: ()=>state.streamingOn?streamingStop():streamingStart()}, "stream"),
        m("details#userlist",
            m("summary#status", `${settings.channel||"lobby"} (${state.users.size} online)`),
            m("div", sorted(state.users).join(', ')),
        ),
    ])
}
const Login = {
    oncreate: growWidth,
    onbeforeremove: shrinkWidth,
    view: () => m("div", [
        m("form[name=login]", {onsubmit: login}, [
            m("input[name=username]", {
                oninput: (e)=>{settings.username=e.target.value},
                value: settings.username,
                autocomplete: "off",
                placeholder: "pick any username!",
            }),
        ]),
    ])
}
const Nav = {
    view: ()=>m("nav",
        state.loggedIn
        ? [m("div", {key:1}), m("button.logout", {onclick: logout}, "log out"), m(Actions)]
        : [m(Login, {key:1}), m("button.login", {onclick: login}, "log in")]
    )
}
const Loading = {
    onbeforeremove: fadeOut,
    view: ()=>m("div.centered-overlay", m("img", {src:"/svg/spinner.svg"}))
}
const Main = {
    async oninit() {
        settings.iceServers = await m.request("/turnservers.json");
    },
    view: ()=> [
        state.loggedIn
        ? [settings.chatOn?m(Chat):null, m(Nav), settings.settingsOn?m(Settings):null, m(Streams)]
        : [m(Nav)],

        state.busy
        ? m(Loading)
        : null,
    ]
}
addEventListener("peerVolume", onPeerVolume);
listen("onStream", onStream);
listen("socketEvent", m.redraw);
listen("login", m.redraw);
listen("logout", m.redraw);
listen("peerUpdate", m.redraw);
addEventListener("socketError", (event:CustomEvent) => {
        if(event.detail&&event.detail.value) { alert(event.detail.value) }
        else { console.log(event) }
        m.redraw();
    }
);
// display a loading screen to sort some race conditions re:animation/websockets
listen("login", ()=>state.busy=false);
listen("socketError", ()=>state.busy=false);
