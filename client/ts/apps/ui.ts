defaults["chatOn"] = true;
defaults["settingsOn"] = false;
const UserStrings = {
    largeFile: "You are uploading a large file. This may disrupt your connection. Proceed?",
    introMessage:
        "**Tip**: Address users privately with `@`, and link to other channels with `#`."
        + "\n\n" + "Use `shift+enter` to quick-post a message."
    ,
}
let state = {
    title: document.title,
    users: new Set(),
    messages: [UserStrings.introMessage],
    uploads: [],
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
function hotkey(e) {
    if(e.shiftKey) switch(e.key) {
        case 'Enter': e.preventDefault(); post(); break;
    }
    else switch(e.key) {
        case 'Tab':
            e.preventDefault();
            let chars = e.target.value.split('');
            chars.splice(e.target.selectionStart, e.target.selectionEnd-e.target.selectionStart, '    ');
            e.target.value = chars.join('');
            break;
    }
}
function changeChannel() {
    let current = settings.channel || "lobby";
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
        style: {
            filter: settings.videoFilter,
            transform: settings.videoTransform,
        },
        playsinline: true,
        autoplay: true,
        controls: settings.controls,
        muted: user === settings.username,
        srcObject: peerStreams[user],
    }
    return m(`div.streamContainer.${user}`,
        m("video", attributes),
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
            m("form[name=login]", {onsubmit: login}, [
                m("input[name=username]", {
                    oninput: (e)=>{settings.username=e.target.value},
                    value: settings.username,
                    autocomplete: "off",
                    placeholder: "pick any username!",
                }),
                m("button.login", {onclick: login}, "log in"),
            ]),
        ])
    }
}
let Upload = {
    view: ()=>[
        m("input#fileInput[type=file][multiple][hidden]", {onchange: uploadFile}),
        state.uploads.map(([name, url, size], i)=>[
                m("a", {download: name, href: url}, `${name} (${size})`),
            ]
        ),
   ]
}
let Chat = {
    oncreate: function(vnode) {
        scrollToNewest();
        let delayed = () => vnode.dom.classList.remove("enter");
        return new Promise((resolve) => {setTimeout(delayed, 500)})
    },
    onbeforeremove: function(vnode) {
        vnode.dom.classList.add("exit");
        return new Promise((resolve) => {setTimeout(resolve, 500)})
    },
    view: () => m("div#chat.enter", [
        m("div#chat-log", state.messages.map(renderPost)),
        m("form#chat-form", {onsubmit: post},
            m("button.clear", {onclick: clear}, "clear"),
            m("textarea[name=text]", {onkeydown: hotkey, autocomplete: "off"}),
            m("button.post", {onclick: post}, "post"),
            m("button.upload", {onclick: upload}, "upload"),
        ),
        m(Upload),
    ])
}
let Settings = {
    view: ()=> m("div.centered-overlay",
        {
            onclick(e){ settings.settingsOn=false },
        },
        [
        m("table#settings",
        {
            onclick(e){ e.stopPropagation() },
        },
        Object.keys(defaults).map(k=>
            m("tr", [
                m("td", k),
                m("td", opts[k]
                ? m("select", {
                        onchange(e){ settings[k] = JSON.parse(e.target.value) },
                        value: JSON.stringify(settings[k]),
                    },
                    [defaults[k], ...opts[k]].map(o => m("option", JSON.stringify(o)))
                    )
                : m("input", {
                        onchange: (e) => settings[k] = JSON.parse(e.target.value),
                        value: JSON.stringify(settings[k]),
                    })
                )
            ]),
        )),
    ])
}
let Nav = {
    view: ()=>m("nav", [
        m("button.logout", {onclick: logout}, "log out"),
        m("button.settings", {style: {opacity: settings.settingsOn?1:0.5}, onclick: ()=>settings.settingsOn=!settings.settingsOn}, "settings"),
        m("button.chat", {style: {opacity: settings.chatOn?1:0.5}, onclick: ()=>settings.chatOn=!settings.chatOn}, "chat"),
        m("button.stream", {style: {opacity: state.streamingOn?1:0.5}, onclick: ()=>state.streamingOn?streamingStop():streamingStart()}, "stream"),
        m("details#userlist",
            m("summary#status", `${settings.channel||"lobby"} (${state.users.size} online)`),
            m("div", sorted(state.users).join(', ')),
        ),
    ])
}
let Main = {
    async oninit() {
        settings.iceServers = await m.request("/turnservers.json");
    },
    view: ()=>
        state.loggedIn
        ? [settings.chatOn?m(Chat):null, m(Nav), settings.settingsOn?m(Settings):null, m(Streams)]
        : [m(Login)]
}
addEventListener("peerVolume", onPeerVolume);
listen("onStream", onStream);
listen("socketEvent", m.redraw);
listen("login", m.redraw);
listen("logout", m.redraw);
listen("peerUpdate", m.redraw);
addEventListener("socketError", (event:CustomEvent) => {
        if(event.detail) { alert(event.detail.value) };
        m.redraw();
    }
);
