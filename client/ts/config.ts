let config = {
    wsUrl: location.href === "http://localhost:8000/"
        ? "ws://localhost:9160/"
        : "wss://chat.roderic.ca/ws/",
}
