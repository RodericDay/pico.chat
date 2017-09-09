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
