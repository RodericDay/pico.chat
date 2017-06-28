let config = {
    wsUrl: location.href.match("http://localhost:8000/")
        ? "ws://localhost:9160/"
        : "wss://chat.roderic.ca/ws/",
    iceServers: [{urls: ['stun:stun.l.google.com:19302']}],
    media: {audio: true, video: {width: 320, height: 240, facingMode: "user"}},
}
