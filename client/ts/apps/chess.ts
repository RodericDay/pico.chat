let baseString = `
♜♞♝♛♚♝♞♜
♟♟♟♟♟♟♟♟
・・・・・・・・
・・・・・・・・
・・・・・・・・
・・・・・・・・
♙♙♙♙♙♙♙♙
♖♘♗♕♔♗♘♖
`;
let tiles = Array(64).fill(null).map((_,i)=>(i+i/8|0)%2?"lightgray":"white");
let pieces = baseString.replace(/\n/g,'').split("").map((c,i)=>[c, i%8, i/8|0, (i/8+i|0)%2]).filter(([c,])=>c!="・");
let css = `
    .board {
        position: fixed;
        bottom: 100px;
        display: grid;
        grid-template-columns: 1fr 1fr 1fr 1fr 1fr 1fr 1fr 1fr;
    }
    .board > * {
        width: 25px;
        height: 25px;
    }
    .piece {
        display: flex;
        align-items: center;
        justify-content: center;
        position: absolute;
    }
`;
let board = document.createElement("board");
board.className = "board";
document.body.appendChild(board);
let makeTile = (color,i) => {
    let attributes = {
        style: {backgroundColor: color},
        ondragover: (e)=> {
            e.preventDefault(); // unclear why this is necessary
        },
        ondrop: (e)=>{
            let [j,x,y] = [+e.dataTransfer.getData("dummy"), i%8, i/8|0];
            pieces[j][1] = x;
            pieces[j][2] = y;
            sendMessage("chessState", pieces);
        },
    }
    return m(`div.tile`, attributes)
}
let makePiece = ([c,x,y,z],i) => {
    let attributes = {
        style: {left: x*25+"px", top: y*25+"px", zIndex: z},
        ondragstart: (e)=>{
            e.dataTransfer.setData("dummy", i);
        },
    };
    return m("div.piece[draggable]", attributes, c)
};
m.mount(board, {view:()=>!state.chessOn?[]:[m("style", css), ...tiles.map(makeTile), ...pieces.map(makePiece)]});
sync("chessState", "pieces");
var chess = () => {state.chessOn = !state.chessOn};
state.actions.push(()=>chess);
