let defaults = {
    title: document.title,
    channel: location.hash,
    username: "",
    loggedIn: false,
    chessOn: false,
    chatOn: true,
    ws: null,
    users: new Set(),
    messages: ["**Tip**: Address users privately with `@`, and link to other channels with `#`."],
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
document.body.appendChild(app);
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
