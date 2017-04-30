function compareZIndex(A, B) {
    return A.z > B.z ? -1 : 1
}

function pointerDown(event) {

    if(event.target === document.body) {
        isSelecting = true;
        lastX = event.pageX;
        lastY = event.pageY;
        event.preventDefault();
    }
}

function pointerMove(event) {
    if(event.buttons === 0) return;

    event.preventDefault();
    var X = event.pageX;
    var Y = event.pageY;

    if (isSelecting) {
        selectArea.style.left = (X < lastX ? X : lastX) + "px";
        selectArea.style.top = (Y < lastY ? Y : lastY) + "px";
        selectArea.style.width = (Math.abs(X - lastX)) + "px";
        selectArea.style.height = (Math.abs(Y - lastY)) + "px";
    }
}

function pointerUp(event) {

    if (isSelecting) {
        isSelecting = false;
        selectArea.style.width = "" + 0;
        selectArea.style.height = "" + 0;
    }
}

function onLoad() {
    document.body.appendChild(selectArea);
}

var selectArea = document.createElement("div");
selectArea.style.position = "absolute";
selectArea.id = "select-area";
selectArea.style.backgroundColor = "blue";
selectArea.style.opacity = "0.5";
selectArea.style.zIndex = "999999";
var isSelecting = false;
var lastX = 0;
var lastY = 0;
var lastZ = 100;

window.addEventListener("load", onLoad);

window.addEventListener("mouseup", pointerUp);
window.addEventListener("mousedown", pointerDown);
window.addEventListener("mousemove", pointerMove);

window.addEventListener("touchstart", pointerDown);
window.addEventListener("touchend", pointerUp);
window.addEventListener("touchmove", pointerMove);
