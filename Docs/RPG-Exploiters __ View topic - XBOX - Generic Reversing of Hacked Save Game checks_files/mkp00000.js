<!--

var B_open = 0;
var I_open = 0;
var U_open = 0;
var QUOTE_open = 0;
var CODE_open = 0;
var HTML_open = 0;

var bbtags   = new Array();

var myAgent   = navigator.userAgent.toLowerCase();
var myVersion = parseInt(navigator.appVersion);

var is_opera = (myAgent.indexOf("opera") != -1);
var is_ie   = ((myAgent.indexOf("msie") != -1)  && (is_opera==false));
var is_nav  = ((myAgent.indexOf('mozilla')!=-1) && (myAgent.indexOf('spoofer')==-1)
                && (myAgent.indexOf('compatible') == -1) && (is_opera==false)
                && (myAgent.indexOf('webtv') ==-1)       && (myAgent.indexOf('hotjava')==-1));

var is_win   =  ((myAgent.indexOf("win")!=-1) || (myAgent.indexOf("16bit")!=-1));
var is_mac    = (myAgent.indexOf("mac")!=-1);



function stacksize(thearray) {
    for (i = 0 ; i < thearray.length; i++ ) {
        if ( (thearray[i] == "") || (thearray[i] == null) || (thearray == 'undefined') ) {
            return i;
        }
    }
    return thearray.length;
}

function pushstack(thearray, newval) {
    arraysize = stacksize(thearray);
    thearray[arraysize] = newval;
}

function popstack(thearray) {
    arraysize = stacksize(thearray);
    theval = thearray[arraysize - 1];
    delete thearray[arraysize - 1];
    return theval;
}

function closeall() {
    if (bbtags[0]) {
        while (bbtags[0]) {
            tagRemove = popstack(bbtags)
            document.editor.ta.value += "[/" + tagRemove + "]";
            if ( (tagRemove != 'FONT') && (tagRemove != 'SIZE') && (tagRemove != 'COLOR') ) { 
                eval("document.editor." + tagRemove + ".value = ' " + tagRemove + " '");
                eval(tagRemove + "_open = 0");
            }
        }
    }
    document.editor.tagcount.value = 0;
    bbtags = new Array();
    document.editor.ta.focus();
}


function add_code(NewCode) {
    document.editor.ta.value += NewCode;
    document.editor.ta.focus();
}

function alterfont(theval, thetag) {
    if (theval == 0)
        return;
    if(doInsert("[" + thetag + "=" + theval + "]", "[/" + thetag + "]", true))
        pushstack(bbtags, thetag);
    document.editor.ffont.selectedIndex  = 0;
    document.editor.fsize.selectedIndex  = 0;
    document.editor.fcolor.selectedIndex = 0;
}

function simpletag(thetag) {
    var tagOpen = eval(thetag + "_open");

    if (tagOpen == 0) {
        if(doInsert("[" + thetag + "]", "[/" + thetag + "]", true))
        {
            eval(thetag + "_open = 1");
            eval("document.editor." + thetag + ".value += '*'");

            pushstack(bbtags, thetag);
        }
    }
    else {
        // Find the last occurance of the opened tag
        lastindex = 0;

        for (i = 0 ; i < bbtags.length; i++ )
        {
            if ( bbtags[i] == thetag )
            {
                lastindex = i;
            }
        }

        // Close all tags opened up to that tag was opened
        while (bbtags[lastindex])
        {
            tagRemove = popstack(bbtags);
            doInsert("[/" + tagRemove + "]", "", false)


            if ( (tagRemove != 'FONT') && (tagRemove != 'SIZE') && (tagRemove != 'COLOR') ) {
                eval("document.editor." + tagRemove + ".value = ' " + tagRemove + " '");
                eval(tagRemove + "_open = 0");
            }
        }
    }
}


function tag_list() {
    var listvalue = "init";
    var thelist = "";
    while ( (listvalue != "") && (listvalue != null) ) {
        listvalue = prompt(list_prompt, "");
        if ( (listvalue != "") && (listvalue != null) ) {
            thelist = thelist+"[*]"+listvalue+"\n";
        }
    }
    if ( thelist != "" ) {
        doInsert( "[LIST]\n" + thelist + "[/LIST]\n", "", false);
    }
}

function tag_url() {
    var FoundErrors = '';
    var enterURL   = prompt(text_enter_url, "http://");
    var enterTITLE = prompt(text_enter_url_name, "My Webpage");

    if (!enterURL) {
        FoundErrors += " " + error_no_url;
    }
    if (!enterTITLE) {
        FoundErrors += " " + error_no_title;
    }
    if (FoundErrors) {
        alert("Error!"+FoundErrors);
        return;
    }
    doInsert("[URL="+enterURL+"]"+enterTITLE+"[/URL]", "", false);
}


function tag_image() {
    var FoundErrors = '';
    var enterURL   = prompt(text_enter_image, "http://");

    if (!enterURL) {
        FoundErrors += " " + error_no_url;
    }
    if (FoundErrors) {
        alert("Error!"+FoundErrors);
        return;
    }
    doInsert("[IMG]"+enterURL+"[/IMG]", "", false);
}

function tag_email() {
    var emailAddress = prompt(text_enter_email, "");
    if (!emailAddress) {
        alert(error_no_email);
        return;
    }
    doInsert("[EMAIL]"+emailAddress+"[/EMAIL]", "", false);
}

function doInsert(ibTag, ibClsTag, isSingle) {
    var isClose = false;
    var obj_ta = document.editor.ta;
    // Ensure it works for IE4up / Win only
    if ((myVersion >= 4) && is_ie && is_win) {
        if(obj_ta.isTextEdit){ // this doesn't work for NS, but it works for IE 4+ and compatible browsers
            obj_ta.focus();
            var sel = document.selection;
            var rng = sel.createRange();
            rng.collapse;
            if((sel.type == "Text" || sel.type == "None") && rng != null){
                if(ibClsTag != "" && rng.text.length > 0)
                    ibTag += rng.text + ibClsTag;
                else if(isSingle)
                    isClose = true;
                rng.text = ibTag;
            }
        }
        else {
            if(isSingle)
                isClose = true;
            obj_ta.value += ibTag;
        }
    }
    else {
        // this should work with Mozillas
        if ( (myVersion >= 4) && is_win && !is_opera) {
            var length = obj_ta.textLength;
            var start = obj_ta.selectionStart;
            var end = obj_ta.selectionEnd;
            var head = obj_ta.value.substring(0,start);
            var rng = obj_ta.value.substring(start, end);
            var tail = obj_ta.value.substring(end, length);
            if( start != end ){
                if (ibClsTag != "" && length > 0)
                    ibTag += rng + ibClsTag;
                else if (isSingle)
                    isClose = true;
                rng = ibTag;
                obj_ta.value = head + rng + tail;
                start = start + rng.length;
            }
            else{
                if(isSingle)
                    isClose = true;
                obj_ta.value = head + ibTag + tail;
                start = start + ibTag.length;
            }
            obj_ta.selectionStart = start;
            obj_ta.selectionEnd = start;
        }
        else {
            if(isSingle)
                isClose = true;
            obj_ta.value += ibTag;
        }
    }
    obj_ta.focus();
    return isClose;
}

function getCookie(name) {
  var dc = document.cookie;
  var prefix = name + "=";
  var begin = dc.indexOf("; " + prefix);
  if (begin == -1) {
    begin = dc.indexOf(prefix);
    if (begin != 0) return null;
  } else
    begin += 2;
  var end = document.cookie.indexOf(";", begin);
  if (end == -1)
    end = dc.length;
  return unescape(dc.substring(begin + prefix.length, end));
}

function getObj(name)
{
  if (document.getElementById)
  {
    if(document.getElementById(name))
      return document.getElementById(name);
    else
      return false;
  }
  else if (document.all)
  {
	if (document.all[name])
      return document.all[name];
    else
      return false;
  }
  else if (document.layers)
  {
    if (document.layers[name])
      return document.layers[name];
    else
      return false;
  }
}

function ColumnClose(currMenu) {
	Mclose = 'menucloseds';
	Mcontent= 'menucontents';
	if (currMenu == 'menudx') {
			Mclose = 'menuclosedr';
			Mcontent = 'menucontentr';
	}

  holder = getObj(currMenu)
  if( holder ){

    if (typeof(window.opera) == 'undefined'
        && typeof(holder.getAttribute) != 'undefined') {
        if (holder.getAttribute("className")) {
            holder.setAttribute("className", Mclose);
        } else {
            holder.setAttribute("class", Mclose);
        }
    }
    else {
        holder.setAttribute("class", Mclose);
    }

    obj = getObj(Mcontent);
    if(obj) obj.style.display = 'none';

    obj = getObj(Mclose);
    if(obj) obj.style.display = '';
  }
}
function ColumnOpen(currMenu) {
  	Mclose = 'menucloseds';
	Mcontent= 'menucontents';
	if (currMenu == 'menudx') {
			Mclose = 'menuclosedr';
			Mcontent = 'menucontentr';
	}
  holder = getObj(currMenu)
  if( holder ){


    if (typeof(window.opera) == 'undefined'
        && typeof(holder.getAttribute) != 'undefined') {
        if (holder.getAttribute("className")) {
            holder.setAttribute("className", currMenu);
        } else {
            holder.setAttribute("class", currMenu);
        }
    }
    else {
        holder.setAttribute("class", currMenu);
    }


    obj = getObj(Mcontent);
    if(obj) obj.style.display = '';

    obj = getObj(Mclose);
    if(obj) obj.style.display = 'none';
  }
}


function MemoPos(name, value) {
   var expire=new Date();
   expire=new Date(expire.getTime()+7776000000);
   document.cookie=  name + "=" +value + "; expires="+expire+"; path=/";

}

function GetPos() {
	var resultsx = getCookie('MKmenusx');
	var resultdx = getCookie('MKmenudx');
	//document.write(result);
	if (resultsx == 1)
	ColumnClose('menusx');
	if (resultdx == 1)
	ColumnClose('menudx');


}



//-->
