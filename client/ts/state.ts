let defaults = {
    username: "",
    loggedIn: false,
    loginError: null,
    ws: null,
}
let state = defaults;
for(let key of Object.keys(defaults)) {
    try {
        var value = localStorage[key];
        if(value.constructor.name !== "String") {
            value = JSON.parse(value);
        }
        if (defaults[key].constructor.name === value.constructor.name) {
            state[key] = value;
        }
    }
    catch(error) {
    }
}
