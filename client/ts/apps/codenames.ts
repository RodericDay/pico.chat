function randomFrom(iterable) {
    let i = Math.floor(Math.random()*iterable.length);
    return iterable.splice(i, 1)[0]
}
function shuffle(iterable) {
    return iterable.map((x,i)=>Math.floor(Math.random()*(iterable.length-i)))
}
function reveal(i) {
    cards[i][2] = true;
    sendMessage("codenamesState", cards)
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

    while(colors.length) {
        let color = randomFrom(colors);
        let word = randomFrom(randomFrom(pairs).split(','));
        cards.push([color, word, false]);
    }

    m.redraw();
}
let cards:[string, string, boolean][] = [];
let Game = {
    view: () => m("svg#grid",  {style:{"max-height":"70vh"}, viewBox:"0 0 5 5"}, cards.map(([color, word, revealed], i) => {
        var [x, y] = [i%5, Math.floor(i/5)];
        var color = revealed||state.username.includes("42")?color:"beige";
        var opacity = revealed?0.1:1;
        return [
            m("rect", {fill:color, x:x, y:y, height:1, width:1, onclick:()=>reveal(i)}),
            m("text", {opacity:opacity, x:x+0.5, y:y+0.5,
                style: {
                    "pointer-events": "none",
                    "user-select": "none",
                },
                "font-size":0.15,
                "text-anchor":"middle",
                "dominant-baseline":"mathematical"}, word),
        ]}),
    ),
}
let gameRoot = document.createElement("div");
document.body.appendChild(gameRoot);
m.mount(gameRoot, Game);
addEventListener("login", deal);
addEventListener("logout", (e)=>{cards=[]; m.redraw();});
addEventListener("connect", (e:CustomEvent)=>{if(cards.length){sendMessage("codenamesState", cards, e.detail.value)}});
addEventListener("codenamesState", (e:CustomEvent)=>{cards=e.detail.value; m.redraw()});
