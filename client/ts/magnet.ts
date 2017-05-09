var pickedUp = null;
function onPointerDown(e) {
    var target = Array.from(document.getElementsByClassName("magnet"))
        .filter(l=>{
            var r = l.getBoundingClientRect();
            return e.pageX>r.left&&e.pageX<r.right&&e.pageY>r.top&&e.pageY<r.bottom;
        }).pop();
    if(target && target.classList.contains("magnet")) {
        e.preventDefault();
        var r = target.getBoundingClientRect();
        pickedUp = {el: target, dx: e.pageX-r.left, dy: e.pageY-r.top};
    }
}
function onPointerMove(e) {
    if(pickedUp) {
        e.preventDefault();
        if(e.buttons === undefined || e.buttons === 1) {
            pickedUp.el.style.position = "fixed";
            pickedUp.el.style.left = `${e.pageX-pickedUp.dx}px`;
            pickedUp.el.style.top = `${e.pageY-pickedUp.dy}px`;
        }
    }
}
function onPointerUp(e) {
    e.preventDefault();
    pickedUp = null;
}
window.ontouchstart = onPointerDown;
window.ontouchmove = onPointerMove;
window.ontouchend = onPointerUp;
window.onmousedown = onPointerDown;
window.onmousemove = onPointerMove;
window.onmouseup = onPointerUp;
