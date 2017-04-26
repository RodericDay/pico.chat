var style = document.createElement("style");
document.head.appendChild(style);
style.innerHTML = `
    .card {
        position: fixed;
        width: 25px;
        height: 40px;
        outline: 1px solid black;
        background-color: grey;
    }
    .selected {
        outline: 2px solid yellow;
    }
    .select-area {
        background-color: blue;
        opacity: 0.5;
        z-index: 999999;
    }
    .faceup {
        background-color: white;
    }
    .faceup:before {
        font-size: 0.7em;
        content: attr(id);
        display: block;
    }
`;
function cardsKeyUp(event) {
    var selected = getCards("selected");
    var minimize = (a, b) => Math.min(a, b);
    var minX = selected.map(card => card.x).reduce(minimize, 9999);
    var minY = selected.map(card => card.y).reduce(minimize, 9999);
    var minZ = selected.map(card => card.z).reduce(minimize, 9999);
    if (event.shiftKey) {
        if (event.key === 'ArrowLeft') {
            selected.forEach(card => card.x -= card.z - minZ);
        }
        if (event.key === 'ArrowRight') {
            selected.forEach(card => card.x += card.z - minZ);
        }
        if (event.key === 'ArrowUp') {
            selected.forEach(card => card.y -= card.z - minZ);
        }
        if (event.key === 'ArrowDown') {
            selected.forEach(card => card.y += card.z - minZ);
        }
        if (event.key === 'S') {
            shuffle(selected).forEach(card => card.z = ++lastZ);
        }
    }
    else {
        if (event.key === 'ArrowLeft') {
            selected.forEach(card => card.x -= 1);
        }
        if (event.key === 'ArrowRight') {
            selected.forEach(card => card.x += 1);
        }
        if (event.key === 'ArrowUp') {
            selected.forEach(card => card.y -= 1);
        }
        if (event.key === 'ArrowDown') {
            selected.forEach(card => card.y += 1);
        }
        if (event.key === 's') {
            selected.forEach(function (card) { card.x = minX; card.y = minY; });
        }
        if (event.key === 'a') {
            getCards("card").forEach(card => card.div.classList.add("selected"));
        }
        if (event.key === 'f') {
            selected.forEach(card => card.div.classList.toggle("faceup"));
        }
        if (event.key === '5') {
            selected.slice(0, -5).forEach(card => card.div.classList.remove("selected"));
        }
    }
}
window.onmousemove = function (event) {
    if (event.buttons !== 1)
        return;
    var X = event.clientX;
    var Y = event.clientY;
    for (var card of getCards("selected")) {
        card.x = X - card.offsetX;
        card.y = Y - card.offsetY;
    }
    if (isSelecting) {
        selectArea.style.left = "" + (X < lastX ? X : lastX);
        selectArea.style.top = "" + (Y < lastY ? Y : lastY);
        selectArea.style.width = "" + (Math.abs(X - lastX));
        selectArea.style.height = "" + (Math.abs(Y - lastY));
    }
};
window.onmousedown = function (event) {
    var card = event.target["card"];
    lastX = event.clientX;
    lastY = event.clientY;
    if (card) {
        card.div.classList.add("selected");
        getCards("selected")
            .sort(compareZIndex)
            .forEach(function (card) {
            card.offsetX = lastX - card.x;
            card.offsetY = lastY - card.y;
            card.z = ++lastZ;
        });
    }
    else {
        isSelecting = true;
    }
};
window.onmouseup = function (event) {
    var cards = getCards("card");
    cards.forEach(card => card.div.classList.remove("selected"));
    if (isSelecting) {
        cards.filter(card => insideA(selectArea, card))
            .forEach(card => card.div.classList.add("selected"));
        isSelecting = false;
        selectArea.style.width = "" + 0;
        selectArea.style.height = "" + 0;
    }
};
function compareZIndex(A, B) {
    return A.z > B.z ? -1 : 1;
}
function insideA(A, B) {
    A = A.getBoundingClientRect();
    B = B.div.getBoundingClientRect();
    return (A.left < B.left) && (B.right < A.right)
        && (A.top < B.top) && (B.bottom < A.bottom);
}
function shuffle(iterable) {
    /* relies on id */
    var map = {};
    iterable.forEach(item => map[item.div.id] = Math.random());
    var compare = (a, b) => map[a.div.id] > map[b.div.id];
    return iterable.sort(compare);
}
function createCard(id) {
    var card = {
        div: document.createElement("div"),
        get x() { return this.div.getBoundingClientRect().left; },
        get y() { return this.div.getBoundingClientRect().top; },
        get z() { return +this.div.style.zIndex; },
        set x(i) { this.div.style.left = i + "px"; },
        set y(i) { this.div.style.top = i + "px"; },
        set z(i) { this.div.style.zIndex = i; },
        set color(string) { this.div.style.color = string; }
    };
    card.div.id = id;
    card.div.style.position = "absolute";
    card.div.classList.add("card");
    card.div.classList.add("faceup");
    card.div["card"] = card;
    card.div.ondblclick = () => card.div.classList.toggle("faceup");
    document.body.appendChild(card.div);
    return card;
}
function getCards(selector) {
    selector = "." + selector;
    return [...document.querySelectorAll(selector)].map(div => div.card);
}
function cardsLoad() {
    selectArea.style.position = "absolute";
    selectArea.classList.add("select-area");
    document.body.appendChild(selectArea);
    for (var i = 0; i < 52; i++) {
        var suit = i / 13 | 0;
        var rank = i % 13;
        var card = createCard('A23456789TJQK'[rank] + '♠♥♣♦'[suit]);
        card.x = 25 + rank * 25;
        card.y = 200 + suit * 40;
        card.z = ++lastZ;
        card.color = ["black", "red"][suit % 2];
    }
}
var selectArea = document.createElement("div");
var isSelecting = false;
var lastX = 0;
var lastY = 0;
var lastZ = 100;
window.addEventListener("load", cardsLoad);
window.addEventListener("keyup", cardsKeyUp);
