let defaults = {
    channel: location.hash,
    username: "",
    loggedIn: false,
    chatOn: true,
    notificationsOn: false,
    ws: null,
    users: new Set(),
    messages: [],
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
