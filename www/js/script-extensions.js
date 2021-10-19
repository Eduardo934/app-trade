
// Set list selected value to input value 
function ItemListSelected(obj){
  var val = obj.innerHTML;
  
  
  document.getElementById("SymbolModuleID-tradeSymbol").value= val;
  
  Shiny.setInputValue("SymbolModuleID-tradeSymbol", val);
}


