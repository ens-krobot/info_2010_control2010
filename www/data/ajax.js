function openHttpRequest() {
    var XMLHttpReq = false;

    if (window.XMLHttpRequest) {
        XMLHttpReq = new XMLHttpRequest();

        if (XMLHttpReq.overrideMimeType)
            XMLHttpReq.overrideMimeType('text/xml');
    }
    else if (window.ActiveXObject) {
        try {
            var XMLHttpReq = new ActiveXObject('Msxml2.XMLHTTP');
        }
        catch (e) {
            try {
                XMLHttpReq = new ActiveXObject('Microsoft.XMLHTTP');
            }
            catch (e) {}
        }
    }

    return XMLHttpReq;
}

function ajaxMakeRequest(url) {
    var httpReq = openHttpRequest();

    httpReq.onreadystatechange = function() { ajaxReceiveResponse(httpReq); }

    if (url.indexOf('#') != -1)
        // # not supported by IE and useless with ajax
        url = url.substring(0, url.indexOf('#'));

    if (url.indexOf('?') != -1)
        url+= '&ajax=1';
    else
        url+= '?ajax=1';

    httpReq.open('GET', url, true);
    httpReq.send(null);
}

function ajaxMakeFormRequest(formID) {
    var httpReq = openHttpRequest();

    httpReq.onreadystatechange = function() { ajaxReceiveResponse(httpReq); }

    var form = document.getElementById(formID);
    var action = form.action;

    if (action.indexOf('#') != -1)
        // # not supported by IE and useless with ajax
        action = action.substring(0, action.indexOf('#'));

    httpReq.open(form.method, action, true);
    httpReq.setRequestHeader('Content-Type', form.enctype);
    httpReq.setRequestHeader('Connection', 'close');

    var request = 'ajax_' + formID + '=1';

    for (var i = 0, elm; i < form.elements.length; i++) {
        elm = form.elements[i];

        if (elm.name == '_charset_')
            elm.value = 'utf-8';

        if (elm.type != 'checkbox' || elm.checked)
            request+= '&' + elm.name + '=' + encodeURIComponent(elm.value);
    }

    httpReq.send(request);
}

function ajaxReceiveResponse(httpReq) {
    if (httpReq.readyState == 4) {
        if (httpReq.status == 200) {
            var resp = httpReq.responseXML;
            var elt = resp.getElementsByTagName('elt');

            for (var i = 0; i < elt.length; i++) {
                tag = elt.item(i);
                document.getElementById(tag.getAttribute('id')).innerHTML = tag.firstChild.data;
            }

            var attr = resp.getElementsByTagName('attr');

            for (var i = 0; i < attr.length; i++) {
                tag = attr.item(i);
                document.getElementById(tag.getAttribute('id')).setAttribute(tag.getAttribute('name'), tag.firstChild.data);
            }

            var headers = httpReq.getAllResponseHeaders();

            if (headers.indexOf("\r\n") != -1)
                headers = headers.split("\r\n");
            else if (headers.indexOf("\n") != -1)
                headers = headers.split("\n");
            else
                headers = headers.split("\r");

            for (var i = 0, header, vals, j, val; i < headers.length; i++) {
                header = headers[i].split(': ');

                if (header[0] == 'X-Ajax-Request') {
                    vals = header[1].split(', ');

                    for (j = 0; j < vals.length; j++) {
                        val = vals[j].split('; ');
                        ajaxMakeRequest(val[0], val[1]);
                    }
                }
                else if (header[0] == 'X-Ajax-Clean') {
                    vals = header[1].split(', ');

                    for (j = 0; j < vals.length; j++)
                        ajaxClean(vals[j]);
                }
            }

            if (document.createEventObject)
                document.body.fireEvent('onload');
            else {
                var evt = document.createEvent('HTMLEvents');
                evt.initEvent('load', false, false);
                document.body.dispatchEvent(evt);
            }
        }
    }
}

function ajaxClean(resultID) {
    document.getElementById(resultID).innerHTML = '';
}
