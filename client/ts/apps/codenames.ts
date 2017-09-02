function randomFrom(iterable) {
    let i = Math.floor(Math.random()*iterable.length);
    return iterable.splice(i, 1)[0]
}
function shuffle(iterable) {
    return iterable.map((x,i)=>Math.floor(Math.random()*(iterable.length-i)))
}
function reveal(i) {
    cards[i][2] = !cards[i][2];
    sendMessage("codenamesState", cards);
}
async function deal() {
    let data = await fetch("words.txt");
    let text = await data.text();
    let pairs = text.trim().split('\n');
    let colors = [
        ...Array(9).fill("steelblue"),
        ...Array(8).fill("crimson"),
        ...Array(7).fill("beige"),
        ...Array(1).fill("gray"),
    ];

    if(cards.length) {
        cards = [];
    }
    else {
        while(colors.length) {
            let color = randomFrom(colors);
            let word = randomFrom(randomFrom(pairs).split(','));
            cards.push([color, word, false]);
        }
    }

    sendMessage("codenamesState", cards);
}
function Card(i, color, word, revealed) {
    let spymaster = state.username.includes("42");
    let style = {
        "text-align": "center",
        "padding": "5px",
        "user-select": "none",
        "text-decoration": revealed ? "line-through" : "",
        "background-color": revealed||spymaster ? color : "beige",
    }
    return m("span", {onclick: ()=>{reveal(i)}, style: style}, word)
}
let Game = {
    view: () => {
        let containerStyle = {
            "background-color": "peru",
            "padding": "2px",
            "position": "fixed",
            "bottom": "100px",
            "left": "0",
            "right": "0",
            "margin": "0 auto",
            "max-width": "800px",
            "font-size": "smaller",
            "font-family": "sans-serif",
        };
        let gridStyle = {
            "display": "grid",
            "grid-template-columns": "1fr 1fr 1fr 1fr 1fr",
            "grid-gap": "1px",
        };
        let rendered = cards.map(([a,b,c], i)=>Card(i,a,b,c));
        let undercover = cards.filter((card)=>card[2]===false);
        let blueLeft = undercover.filter((card)=>card[0]==="steelblue").length;
        let redLeft = undercover.filter((card)=>card[0]==="crimson").length;
        return !cards.length?[]:m("div", {style: containerStyle}, [
            `${blueLeft} blue, ${redLeft} red left to go`,
            m("div.grid", {style: gridStyle}, rendered)
        ])
    }
}
let cards:[string, string, boolean][] = [];
state.actions.push(deal);
let gameRoot = document.createElement("div");
document.body.appendChild(gameRoot);
m.mount(gameRoot, Game);
sync("codenamesState", "cards");
addEventListener("logout", (e)=>{cards=[]; m.redraw();});
