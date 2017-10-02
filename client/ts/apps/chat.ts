marked.setOptions({sanitize: true});
function renderPost(string) {
    string = string.replace(/ (#\w+)/, (m, g)=>` [${g}](${g})`);
    return m.trust(marked(string).replace(/a href/g, `a target="_blank" href`))
}
function login(event) {
    event.preventDefault();
    openConnection(settings.username, state.channel);
}
function logout() {
    ws.close();
}
function clear() {
    var msg = `You sure you want to delete ${state.messages.length} messages?`
    if(confirm(msg)) {
        state.messages = [];
        localStorage.removeItem("messages");
    }
}
function post(e) {
    e.preventDefault();
    let text = document.getElementById("chat-form")["text"];
    let [msg, target] = text.value.match(/^@(\w+)/)||[text.value,null];
    if(target&&text.value.trim()===`@${target}`) {
        // pass, assume user still typing
    }
    else if(target&&!state.users.has(target)) {
        alert(`${target} not in channel.`)
    }
    else if(target) {//private
        wire("post", text.value, target);
        wire("post", text.value, settings.username); // self
        text.value = `@${target} `;
    }
    else if(text.value) {//public
        wire("post", text.value);
        text.value = "";
    }
}
function scrollToNewest() {
    var _ = function() {
        var el = document.getElementById("chat-log");
        if(el) { el.scrollTop = el.scrollHeight; }
    }
    window.setTimeout(_, 100);
}
/* listeners */
addEventListener("login", (e:CustomEvent)=>{
    let strings = e.detail.value ? e.detail.value.split(';') : [];
    state.users = new Set(strings);
    m.redraw();
    scrollToNewest();
});
addEventListener("connect", (e:CustomEvent)=>{
    state.users.add(e.detail.value);
    m.redraw();
});
addEventListener("disconnect", (e:CustomEvent)=>{
    state.users.delete(e.detail.value);
    m.redraw();
});
addEventListener("post", (e:CustomEvent)=>{
    if(!document.hasFocus()&&document.title===state.title){document.title+=' (!)'}
    if(document.hasFocus()!==settings.chatOn){beep()};
    state.messages.push(`${e.detail.sender}: ${e.detail.value}`);
    localStorage.messages = JSON.stringify(state.messages);
    m.redraw();
    scrollToNewest();
});
addEventListener("focus", (e)=>{
    document.title = state.title;
});
