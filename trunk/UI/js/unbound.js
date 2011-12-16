function sortGrid(table, order) {
	// Remove all characters in c from s.
	var stripChar = function(s, c) {
		var r = "";
		for(var i = 0; i < s.length; i++) {
			r += c.indexOf(s.charAt(i))>=0 ? "" : s.charAt(i);
		}
		return r;
	}
	
	// Test for characters accepted in numeric values.
	var isNumeric = function(s) {
		var valid = "0123456789.,- ";
		var result = true;
		var c;
		for(var i = 0; i < s.length && result; i++) {
			c= s.charAt(i);
			if(valid.indexOf(c) <= -1) {
				result = false;
			}
		}
		return result;
	}

	// Sort table rows.
	var asc = order == "asc";
	var rows = $(table).find("tbody > tr").get();
	var column = $(table).parent(".bDiv").siblings(".hDiv").find("table tr th").index($("th.sorted", ".flexigrid:has(" + table + ")"));
	rows.sort(function(a, b) {
		var keyA = $(asc? a : b).children("td").eq(column).text().toUpperCase();
		var keyB = $(asc? b : a).children("td").eq(column).text().toUpperCase();
		if((isNumeric(keyA)||keyA.length<1) && (isNumeric(keyB)||
			keyB.length<1)) {
			keyA = stripChar(keyA,", ");
			keyB = stripChar(keyB,", ");
			if(keyA.length < 1) keyA = 0;
			if(keyB.length < 1) keyB = 0;
			keyA = new Number(parseFloat(keyA));
			keyB = new Number(parseFloat(keyB));
		}
		return keyA>keyB ? 1 : keyA<keyB ? -1 : 0;
	});

	// Rebuild the table body.
	$.each(rows, function(index, row) {
		$(table).children("tbody").append(row);
	});
	// Clear sorted 
	$(table).find("td.sorted").removeClass("sorted");
	$(table).find("tr").each( function(){
		//Add sorted class to sorted column cells.
		$(this).find("td:nth(" + column + ")").addClass("sorted");
	}); 
} 
  
function reloadGrid(){
	if (timeout<30000) timeout +=100;
	$('.downloads-table').flexReload();
};

function postCommand(command){
	if(selectedRow && (command == "start" || command == "stop" || command == "pause" || command == "prioritydown" || command == "priorityup" || command == "delete"))
	{
		$.post("/ajax", {command: command, row: selectedRow.replace("row", "")},
			function(data) {
				console.log(data);
				clearTimeout(timer);
				reloadGrid();
		});
	}
	
	if (command == "settings" || command == "exit"){
		$.post("/ajax", {command: command},
			function(data) {
				alert(data);
				clearTimeout(timer);
				reloadGrid();
		});
	}
	
	if (command == "new"){
		$("span.error-torrent").text("Loading").show();
		$.post("/ajax", {command: command, url: $("input.new-torrent").val(), path: $("input.new-path").val()},
			function(data) {
			if (data == "done")
			{
				closePopUp();
				clearTimeout(timer);
				reloadGrid();
			} else {
				console.log(data);
				$("span.error-torrent").text(data).show();
			}
		});
	}
};

function closePopUp(){
	//Close Popups and Fade Layer
	$("span.error-torrent").text("").hide()
	$('#fade , .popup_block').fadeOut(function() {
		$('#fade, a.close').remove();  //fade them both out
	});
}

function enableButtons(){
	$("#button-bar div a").addClass("enabled");
	$("#button-bar div a.hard-disabled").removeClass("enabled");
	$("#button-bar div a.row-action").removeClass("disabled");
	$("#button-bar div a.hard-disabled").addClass("disabled");
}

function disableButtons(){
	$("#button-bar div a.row-action").removeClass("enabled");
	$("#button-bar div a.row-action").addClass("disabled");
}

var outerLayout, innerLayout, innerInnerLayout, gridorder;

$(document).ready(function() {
	gridorder = "asc";
	filter = "all";
	enableButtons();
	disableButtons();
	$('#target').submit(function() {
		postCommand("new");
		return false;
	});
	
	$('a.poplight[href^=#]').click(function() {
			var popID = $(this).attr('rel'); //Get Popup Name
			var popURL = $(this).attr('href'); //Get Popup href to define size

			//Pull Query & Variables from href URL
			var query= popURL.split('?');
			var dim= query[1].split('&');
			var popWidth = dim[0].split('=')[1]; //Gets the first query string value

			//Fade in the Popup and add close button
			$('#' + popID).fadeIn().css({ 'width': Number( popWidth ) }).prepend('<a href="#" class="close"><img src="img/close_pop.png" class="btn_close" title="Close Window" alt="Close" /></a>');

			//Define margin for center alignment (vertical   horizontal) - we add 80px to the height/width to accomodate for the padding  and border width defined in the css
			var popMargTop = ($('#' + popID).height() + 80) / 2;
			var popMargLeft = ($('#' + popID).width() + 80) / 2;

			//Apply Margin to Popup
			$('#' + popID).css({
				'margin-top' : -popMargTop,
				'margin-left' : -popMargLeft
			});

			//Fade in Background
			$('body').append('<div id="fade"></div>'); //Add the fade layer to bottom of the body tag.
			$('#fade').css({'filter' : 'alpha(opacity=80)'}).fadeIn(); //Fade in the fade layer - .css({'filter' : 'alpha(opacity=80)'}) is used to fix the IE Bug on fading transparencies 

			return false;
	});

	$('a.close, #fade').live('click', function() { //When clicking on the close or fade layer...
		closePopUp();
		return false;
	});
	
	$("#divdeps").dialog({
		autoOpen: false,
		show: 'slide',
		resizable: false,
		position: 'center',
		stack: true,
		height: 'auto',
		width: 'auto',
		modal: true
	});

	$('html').mousemove(function(){
		if (timeout > 2000)
		{
			timeout = 1500;
			clearTimeout(timer);
			reloadGrid();
		}
		
		timeout = 1500;
	});
	
	$('.outer-west ul li').click(function(){
		filter = $(this).attr('title');
		selectedRow = null;
		clearTimeout(timer);
		reloadGrid();
		$(this).addClass("selected");
		$(this).siblings().removeClass("selected");
	});
	
	
	$('.downloads-table').flexigrid({
		onChangeSort: function(name, order) {
			gridorder = order;
			sortGrid(".downloads-table", order);
		},
		url: '/ajax',
		dataType: 'xml',
		colModel : [
			{display: '#',			name : 'priority',	width : 030, sortable : true, align: 'left'},
			{display: 'Name',		name : 'name',		width : 190, sortable : true, align: 'left'},
			{display: 'Size',		name : 'size',		width : 060, sortable : true, align: 'left'},
			{display: 'Done',		name : 'percent',	width : 080, sortable : true, align: 'left'},
			{display: 'Status',		name : 'status',	width : 080, sortable : true, align: 'left'},
			{display: 'Peers',		name : 'peers',		width : 050, sortable : true, align: 'left'},
			{display: 'Down Speed', name : 'downspeed',	width : 080, sortable : true, align: 'left'},
			{display: 'Up Speed',	name : 'upspeed',	width : 080, sortable : true, align: 'left'},
			{display: 'ETA',		name : 'eta',		width : 070, sortable : true, align: 'left'},
			{display: 'Uploaded',	name : 'uploaded',	width : 070, sortable : true, align: 'left'}
		],
		height:'auto',
		striped: false,
		singleSelect: true,
		sortname: "priority",
		sortorder: "asc",
		resizable: true
	});
	
	sortGrid(".downloads-table", "asc");
	
	outerLayout = $('body').layout({
		center__paneSelector: ".outer-center"
		, west__paneSelector: ".outer-west"
		, west__size: 125
		, west__minSize: 100
		, west__maxSize: 200
		, spacing_open: 8 // ALL panes
		, spacing_closed: 12 // ALL panes
		//, north__spacing_open: 0
		//, south__spacing_open: 0
		, north__maxSize: 200
		, south__maxSize: 200 
	});
	
	innerLayout = $('div.outer-center').layout({
		center__paneSelector: "#downloads-field"
		, north__paneSelector: "#button-bar"
		, north__size: 40
		, north__spacing_open: 0
		, north__togglerLength_open: 0
		, north__resizable: false
		, north__slidable: false 
		, south__paneSelector: ".east-south"
		, south__spacing_closed: 12
		, spacing_open: 8 // ALL panes
		, spacing_closed: 8 // ALL panes
		, south__size: 200
		, south__minSize: 100
		, south__maxSize: 350
	});
	
	$('.loading').hide();
});
