const UserStrings = {
    largeFile: "You are uploading a large file. This may disrupt your connection. Proceed?",
    introMessage: "**Tip**: Address users privately with `@`, and link to other channels with `#`.",
}
let defaults = {
    title: document.title,
    channel: location.hash,
    username: "",
    loggedIn: false,
    users: new Set(),
    messages: [UserStrings.introMessage],
    uploads: [],
    status: "",
    chatOn: false,
    settingsOn: false,
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
