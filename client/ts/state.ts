const UserStrings = {
    largeFile: "You are uploading a large file. This may disrupt your connection. Proceed?",
    introMessage: "**Tip**: Address users privately with `@`, and link to other channels with `#`.",
}
let defaults = {
    title: document.title,
    channel: location.hash,
    username: "",
    loggedIn: false,
    chessOn: false,
    chatOn: true,
    ws: null,
    users: new Set(),
    messages: [UserStrings.introMessage],
    uploads: [],
    status: "",
    peers: <{[username: string]: RTCPeerConnection}>{},
    streams: <{[username: string]: MediaStream}>{},
}
let state = defaults;
for(let key of Object.keys(defaults)) {
    try {
        var value = JSON.parse(localStorage[key]);
        if (defaults[key].constructor.name === value.constructor.name) {
            state[key] = value;
        }
    }
    catch(error) {
    }
}
let app = document.createElement("div");
app.id = "app";
document.body.appendChild(app);
let StatusBar = {
    view: ()=>m("footer", [
        m("img", {src: "svg/chess.svg", style: {opacity: state.chessOn?1:0.5}, onclick: ()=>state.chessOn=!state.chessOn, title: "chess"}),
        m("img", {src: "svg/deal.svg", style: {opacity: cards.length?1:0.5}, onclick: deal, title: "codenames"}),
        m("img", {src: "svg/stream.svg", style: {opacity: isEmpty(state.streams)?1:0.5}, onclick: ()=>isEmpty(state.streams)?streamingStop():streamingStart(), title: "stream"}),
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
addEventListener("load", ()=>{
    m.mount(app, Main)
    if(state.username){openConnection()};
});
