function onPeerVolume(e:CustomEvent) {
    let div = document.querySelector(`.streamContainer .info.${e.detail.sender}`);
    let isSpeaking = e.detail.value;
    if(div) {
        isSpeaking ? div.classList.add("loud") : div.classList.remove("loud");
    }
}
var viewStream = (username) => {
    var localConfig = {
        srcObject: peerStreams[username],
        autoplay: true,
        muted: username === state.username,
    }
    return m("div.streamContainer",
        m("video", localConfig),
        m(`div.info.${username}`, username),
    )
}
let Streams = {
    view: ()=>
        m("div#streamGrid", Object.keys(peerStreams).sort().map(viewStream))
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
        m("img", {src: "svg/chess.svg", style: {opacity: state.chessOn?1:0.5}, onclick: ()=>state.chessOn=!state.chessOn, title: "chess"}),
        m("img", {src: "svg/deal.svg", style: {opacity: cards.length?1:0.5}, onclick: deal, title: "codenames"}),
        m("img", {src: "svg/stream.svg", style: {opacity: isEmpty(peerStreams)?1:0.5}, onclick: ()=>isEmpty(peerStreams)?streamingStop():streamingStart(), title: "stream"}),
        m("img", {src: "svg/chat.svg", style: {opacity: state.chatOn?1:0.5}, onclick: ()=>state.chatOn=!state.chatOn, title: "chat"}),
        m("img", {src: "svg/logout.svg", onclick: logout, title: "log out"}),
    ])
}
let Main = {
    view: ()=>
        state.loggedIn
        ? [m(StatusBar), m(Chat), m(Streams), m(Codenames), m(Chess)]
        : [m(Login)]
}
addEventListener("peerVolume", onPeerVolume);
addEventListener("peerUpdate", (e)=>{m.redraw()});
addEventListener("disconnect", (e:CustomEvent)=>{closePeer(e.detail.value)});
addEventListener("logout", streamingStop);
addEventListener("socketEvent", (e:CustomEvent) => {
    m.redraw();
});
addEventListener("socketError", (e:CustomEvent) => {
    alert(e.detail.value);
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
