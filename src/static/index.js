// pull in desired CSS/SASS files
require( './styles/main.scss' );
var $ = jQuery = require( '../../node_modules/jquery/dist/jquery.js' );           // <--- remove if jQuery not needed
require( '../../node_modules/bootstrap-sass/assets/javascripts/bootstrap.js' );   // <--- remove if Bootstrap's JS not needed 

// inject bundled Elm app into div#main
var Elm = require( '../elm/Main' );
var app = Elm.Main.embed( document.getElementById( 'main' ) );
if (window.DeviceMotionEvent) {
    console.log("DeviceMotionEvent supported")
    window.addEventListener('devicemotion', deviceMotionHandler, false);
}
function deviceMotionHandler(eventData) {
     console.log("Resize");
     app.ports.rerollDice.send(null);
}