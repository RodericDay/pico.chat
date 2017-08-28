let config = {
    wsUrl: location.href.match("http://localhost:8000/")
        ? "ws://localhost:9160/"
        : "wss://permanentsignal.com/ws/",
    iceServers: [
        {urls: ['stun:stun.l.google.com:19302']},
        {urls: ['turn:159.203.33.68:3478'], username: 'bionic', credential: 'hunter2'},
    ],
    media: {audio: true, video: {width: 320, height: 240, facingMode: "user"}},
}
