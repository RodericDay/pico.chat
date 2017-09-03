let defaults = {
    title: document.title,
    channel: location.hash,
    username: "",
    loggedIn: false,
    chessOn: false,
    chatOn: true,
    ws: null,
    users: new Set(),
    messages: ["**Tip**: Address a user privately with `@`, and create new channels with `#`."],
    actions: [],
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
