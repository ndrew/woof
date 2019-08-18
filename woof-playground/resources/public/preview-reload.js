var socket = new WebSocket("ws://localhost:8001/ws");

document.addEventListener("DOMContentLoaded", function() {
    document.body.style.background = '#' + Math.floor(Math.random()*16777215).toString(16);
});


socket.onmessage = function(event) {
    console.warn(event.data, (new Date()).getTime());
    if (event.data == 'reload') {
        // 1000 = "Normal closure" and the second parameter is a
        // human-readable reason.
        socket.close(1000, "Reloading page after receiving build_complete");

        console.log("Reloading page after receiving build_complete");
        location.reload(true);
    }

    /*var data = JSON.parse(event.data);
    switch(data.type) {
        case "build_complete":
            // 1000 = "Normal closure" and the second parameter is a
            // human-readable reason.
            socket.close(1000, "Reloading page after receiving build_complete");

            console.log("Reloading page after receiving build_complete");
            location.reload(true);

            break;

        default:
            console.log(`Don't know how to handle type '${data.type}'`);
    }*/
}