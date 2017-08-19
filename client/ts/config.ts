let config = {
    wsUrl: location.href.match("http://localhost:8000/")
        ? "ws://localhost:9160/"
        : "wss://chat.roderic.ca/ws/",
    iceServers: [
        {urls: ["159.203.33.68:3478"], username: 'test', password: 'test'},
    ],
    media: {audio: true, video: {width: 320, height: 240, facingMode: "user"}},
}
