function getResource(path, callback) {
    var xhr = new XMLHttpRequest();
    xhr.open('GET', path);
    xhr.onload = event => callback(xhr.responseText);
    xhr.send();
}
function shuffle(array) {
    for (var i = 0; i < array.length; i++) {
        var r = Math.floor(Math.random() * (array.length - 1));
        array.push(array.splice(r, 1)[0]);
    }
}
function setupChess() {
    [...`
    ♜♞♝♛♚♝♞♜
    ♟♟♟♟♟♟♟♟
    ・・・・・・・・
    ・・・・・・・・
    ・・・・・・・・
    ・・・・・・・・
    ♙♙♙♙♙♙♙♙
    ♖♘♗♕♔♗♘♖
    `].filter(c=>c!=='\n').forEach(function(s, i){
        if(s !== "・") {
            magnets.push([["div.magnet.piece[text=${s}]"], 40*(i%8), 40*(i/8|0), i]);
        }
    });
}
function setupCodenames() {
    getResource('words.txt', function(text){
        text.split('\n').reverse().forEach(function(t, i) {
            if (t) {
                var states = t.split(',').map(s=>`div.magnet.card[text=${s}]`);
                magnets.push([states, -5, 5, i]);
            }
        });
        var p1 = Math.random() > 0.5 ? "blue" : "red";
        var squares = [
            p1, "black",
            ...Array(8).fill("blue"),
            ...Array(8).fill("red"),
            ...Array(7).fill("gray")
        ];
        for (var color of squares) {
            magnets.push([[`div.magnet.card.${color}`], -5, 5, 2000]);
        }
        /* little trick to generate a dynamic image */
        var canvas = document.createElement("canvas");
        canvas.width = 60;
        canvas.height = 60;
        var ctx = canvas.getContext("2d");
        ctx.fillStyle = p1;
        ctx.fillRect(0, 0, 60, 60);
        var src1 = canvas.toDataURL();
        ctx.fillStyle = "white";
        ctx.fillRect(2, 2, 56, 56);
        shuffle(squares);
        squares.forEach((color, i) => {
            ctx.fillStyle = color;
            ctx.fillRect(3 + 11 * (i % 5), 3 + 11 * (i / 5 | 0), 10, 10);
        });
        var src2 = canvas.toDataURL();
        magnets.push([[`img.magnet[src=${src1}]`, `img.magnet[src=${src2}]`], 50, 50, 1000]);
    });
}
