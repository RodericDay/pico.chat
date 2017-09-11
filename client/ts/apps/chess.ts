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
        transition: transform 0.5s;
        transform: translate(0px, 0px);
    }
`;
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
        style: {transform: `translate(${x*25}px, ${y*25}px`, zIndex: z},
        ondragstart: (e)=>{
            e.dataTransfer.setData("dummy", i);
        },
    };
    return m("div.piece[draggable]", attributes, c)
};
let Chess = {
    view:()=>!state.chessOn?[]:[m("style", css), m(".board", ...tiles.map(makeTile), ...pieces.map(makePiece))]
}
sync("chessState", "pieces");
var chess = () => {state.chessOn = !state.chessOn};
