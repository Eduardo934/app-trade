
// Add elements to dropdown list 
shinyjs.AddListContent = function(params){
$(function() {
    var data= params[0];
    
    $('#dropdown-content').empty();
    
    if(Array.isArray(data) === false){
     return;
    }
    
    function addCode() {
               
               
               for(var i = 0; i< data.length; i++){
               
                 document.getElementById("dropdown-content").innerHTML += 
                "<a onClick= 'ItemListSelected(this)'>" + data[i] + "</a>";
               }
             }
             
    addCode();
    
});

};