// ShinySprites is javascript code written by 
// Peter Jackson, Professor, Engineering Systems and Design,
// Singapore University of Technology and Design, May, 2021.
// You are welcome to use, modify and publish this code. Source attribution is appreciated.

// The purpose is to extend R Shiny to support sprite animation.

// In your R shiny code, provide the following ui statements:
//                tags$script(src="ShinySprites.js"),
//                tags$div(id="playingfield"), 
//                tags$script(src="https://d3js.org/d3.v3.min.js"),
//                # Server side has control over the style of sprite label and border
//                tags$style(".spritelabel {fill:yellow;} .spriteborder {stroke:red;stroke-width:2} ")

// There are three R Shiny messages supported: 
// "initPlayingField"
// "displaySprites"
// "setAnimationDuration"

// In your R Shiny sever code you can listen for click events:
//                observeEvent(input$spriteClick,{})
//                observeEvent(input$playingFieldClick,{})


// initialize a global variable spritehome with default playing field dimensions and animation duration
// If you want to create multiple playing fields, perhaps turn this into an array of spritehomes.
var spritehome = {svgid:'spritehome',width:1728,height:972,animDuration:200};

// Initialize a global variable spritedata as an empty array.
var spritedata = [];

// Trick for measuring the length of a string in pixels
// https://blog.mastykarz.nl/measuring-the-length-of-a-string-in-pixels-using-javascript/

Shiny.addCustomMessageHandler("initPlayingField",
	function(message){
    // We assume the message is an array of objects.
    // We assume the first object contains the id of a div created by the server 
    // which we can modify from the client side.
		var divID = message[0].divid;
		// We assume the first object also contains an image file name and dimensions for the playing field.
		var imgname = message[0].imgname;
		var height = message[0].height;
		var width = message[0].width;
		// Save the dimensions for later use
		spritehome.width = width;
		spritehome.height = height;
		// Get the name of the new svg element from the spritehome variable.
		var svgid = spritehome.svgid;
		//console.log("The div id is: ",divID);
		// Create HTML to have a hidden ruler (for measuring string widths) and an svg element of 
		// the desired size.
		// Note that the hidden ruler is given a class 'spritelabel' which can be controlled with CSS from the server
		var newhtml =  "<span id='hiddenruler' class='spritelabel' style='visibility:hidden;whitespace:nowrap'>"
		          +"</span><svg id='"+svgid+"' width='" + width+"' height='"+height+"' >" 
              + "<image width='"+width+"' height='"+height+"' xlink:href='"+imgname+"' />"
              +"</svg>";
    // Display the html string for debugging purposes
   // console.log(newhtml); 
    // Initialize the playing field
		document.getElementById(divID).innerHTML = newhtml;
		// Set up a listener for a click event on the svg
		d3.select("#"+svgid).on("click",function(){
		  var coords = d3.mouse(this);
		  //console.log(coords);
		  //Express coordinates as a percentage of playing field dimensions
		  //var coords_pct = [coords[0]/spritehome.width*100,coords[1]/spritehome.height*100];
		  
		  var coords_pct = [coords[0],coords[1]];
		  
		  //console.log(coords_pct);
		  // Pass the click coordinates, in percentage terms, to the server
		  Shiny.setInputValue("playingFieldClick",coords_pct);
		});
	});

// Given a string of text, determine how wide it will be when styled as spritelabel	
getTextPixelLength =function(text){
  // Recall we defined the hiddenruler inside the playing field div and styled it as spritelabel
  var hiddenruler = document.getElementById("hiddenruler");
  // Assign our text to the innerHTML of the hiddenruler
  hiddenruler.innerHTML = text;
  // Capture and return the width in pixels 
  return hiddenruler.offsetWidth;
};


// Compute coordinates of a single sprite from a datum passed by the server	
getSpriteAttributes = function(spritedatum){
  var spriteAttributes = {};
  
  spriteAttributes.x = spritedatum.x_pct ;
  spriteAttributes.y = spritedatum.y_pct ;
  //spriteAttributes.x = spritedatum.x_pct /100 * spritehome.width;
  //spriteAttributes.y = spritedatum.y_pct /100 * spritehome.height;
  
  spriteAttributes.width = spritedatum.x_scalepct ;
  spriteAttributes.height = spritedatum.y_scalepct ;
  var labelWidth = getTextPixelLength(spritedatum.label);
  // The label coordinates are offsets from the x,y coordinates of the sprite
  
  spriteAttributes.label_x =(spriteAttributes.width-labelWidth)/2 ; // center the label text
  spriteAttributes.label_y =spriteAttributes.height ; // place the label below the image

  return spriteAttributes;
};

Shiny.addCustomMessageHandler("displaySprites",
  function(newspritedata){
    // This is function called by the server to display an array of displaySprites
    // There are lots of debugging statements here which you can uncomment to see the behavior
    //console.log("New sprite data: "+JSON.stringify(newspritedata));
    // create a list of the old sprite fields
    var oldids = spritedata.map(function(d){return d.id;});
    // create a list of the new spriteids
    var newids = newspritedata.map(function(d){return d.id;});
    // create a list of exiting sprite ids
    var exitids = oldids.filter(function(d){return!newids.includes(d);});
    // create a list of entering sprite ids
    var enterids = newids.filter(function(d){return !oldids.includes(d);});
    // For debugging, print these lists to the console
    //console.log("oldids: "+JSON.stringify(oldids));
    //console.log("newdids: "+JSON.stringify(newids));
    //console.log("exitids: "+JSON.stringify(exitids));
    //console.log("enterids: "+JSON.stringify(enterids));
    // Get the name of the new svg element from the spritehome variable.
		var svgid = spritehome.svgid;
		// Select the svg element
    var svghome = d3.select("#"+svgid);
    //console.log("oldsprites: "+JSON.stringify(svghome.selectAll(".sprite")));
    // Attach the old spritedata to everything in the svg which has class 'sprite'.
    // (There should be a one-to-one match.)
    var oldsprites = svghome.selectAll(".sprite").data(spritedata);
    // Filter out the sprites which need to be removed
    var exitingsprites = oldsprites.filter(function(d){return exitids.includes(d.id);});
    // Remove the sprites that will not match ids in the new sprite data
    exitingsprites.remove();
    // Discard the old sprite data and replace it with the new sprite data for ids that match old ids
    spritedata = newspritedata.filter(function(d){return oldids.includes(d.id);});
    //console.log("spritedata: "+JSON.stringify(spritedata));
    var enteringsprites = newspritedata.filter(function(d){return  enterids.includes(d.id);});
    //console.log("enteringspites: "+JSON.stringify(enteringsprites));
    spritedata = spritedata.concat(enteringsprites);
    //console.log("spritedata: "+JSON.stringify(spritedata));
    var allsprites = svghome.selectAll(".sprite").data(spritedata);
    allsprites.exit().remove(); //Not sure this is needed.
    //console.log("allsprites: "+JSON.stringify(allsprites));

    // For each datum in the data array which is new, append an svg group ('g') of class 'sprite'
    var newsprites = allsprites.enter().append("g").attr("class","sprite");
    
    // To each new sprite group, append an image with width, height, and img determined by the matching datum
    newsprites.append("svg:image")
  	 .attr("x","0px")
  	 .attr("y","0px")
  	 .attr("width",function(d){var spriteAttributes = getSpriteAttributes(d); 
  	                            return spriteAttributes.width+"px";})
  	 .attr("height",function(d){var spriteAttributes = getSpriteAttributes(d); 
  	                            return spriteAttributes.height+"px";})
  	 .attr("xlink:href",function(d){return d.img});
  	
  	// To each new sprite group, append text with class 'spritelabel' and position and label determined
  	// by the matching datum. 
  	// The server can modify the CSS for 'spritelabel' to change the appearance of the label
  	 newsprites.append("text")
  	 .attr("class","spritelabel")
  	 .attr("x",function(d){var spriteAttributes = getSpriteAttributes(d); 
  	                        return spriteAttributes.label_x+"px";})
  	 .attr("y",function(d){var spriteAttributes = getSpriteAttributes(d); 
  	                        return spriteAttributes.label_y+"px";})
  	 .text(function(d){return d.label;});

  	// To each new sprite group, append a rectangle with class 'spriteborder' and 
  	// set its dimensions as determined by the matching datum. 
  	// The server can modify the CSS for 'spriteborder' to change the appearance of the border 
  	// (color and thickness) .
  	// The stroke is either transparent or fully opaque depending on the setting of 'showBorder' in the datum
  	 newsprites.append("rect")
  	 .attr("class","spriteborder")
  	 .attr("x","0px")
  	 .attr("y","0px")
  	 .attr("width",function(d){var spriteAttributes = getSpriteAttributes(d); 
  	                            return spriteAttributes.width+"px";})
  	 .attr("height",function(d){var spriteAttributes = getSpriteAttributes(d); 
  	                            return spriteAttributes.height+"px";})
  	 .style("stroke-opacity",function(d){return d.showBorder?1:0;}) 
  	 .style("stroke", "#3F82EF" )
  	 .style("fill-opacity",0.0);  // the rectangle fill should be transparent
  	 
  	 // The whole sprite group can be translated to a position determined by the datum
  	 newsprites.attr("transform",function(d){var spriteAttributes = getSpriteAttributes(d);
                                            return "translate("+spriteAttributes.x+","+spriteAttributes.y+")";
    });
    
    newsprites.on("click", function(d){
          console.log(JSON.stringify(d));
          // Get current event info
          //console.log(d3.event);
          d3.event.stopPropagation();  // prevent click on svg background
          // Get x & y co-ordinates
          //console.log(d3.mouse(this));
          // Send the datum of the sprite back to Shiny (it will be encoded as JSON automatically)
          Shiny.setInputValue("spriteClick",d);
    });
  	 
    // Loop through all sprites and change the attributes of the image, the label, and the rectangle
    // to match the new data
  	 allsprites.each(function(d,i){
  	   //console.log("i:"+i+" d: "+JSON.stringify(d));
  	   var spriteAttributes = getSpriteAttributes(d);
  	   var img = d3.select(this).select('image');
  	   img.attr("width",spriteAttributes.width+"px")
  	    .attr("height",spriteAttributes.height+"px")
  	   .attr("xlink:href",function(d){return d.img;});
  	   
  	   var txt = d3.select(this).select('text');
  	   txt.attr("x",spriteAttributes.label_x+"px")
  	    .attr("y",spriteAttributes.label_y+"px")
  	    .text(function(d){return d.label;});
  	    
  	    var rect = d3.select(this).select('rect');
  	    rect.attr("width",spriteAttributes.width+"px")
  	    .attr("height",spriteAttributes.height+"px")
  	    .style("stroke-opacity",function(d){return d.showBorder?1:0;})
    	  .style("stroke", "#3F82EF" );
  	    
      });
      
      // Get the current animation duration
  	 	var animationDuration = spritehome.animDuration;
  	 	// Animate the transition of translating the location of the sprites to their new locations
  	 	allsprites.transition()
  	 	 	.attr("transform",function(d){var spriteAttributes = getSpriteAttributes(d);
                                      return "translate("+spriteAttributes.x+","+spriteAttributes.y+")";})
  	    .duration(animationDuration).ease('linear'); 

});

Shiny.addCustomMessageHandler("setAnimationDuration",
  function(animationdata){
    // Recall our convention is to pass all data from the server as a data frame
    // We assume animationdata is an array of objects, 
    // the first of which has a name-value pair for "animationDuration"
    var duration = animationdata[0].animationDuration;
    //console.log("New animation duration: "+duration);
    spritehome.animDuration = duration;
  });


