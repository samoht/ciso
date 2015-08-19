function main () {

	var base = "http://127.0.0.1:8080/";
	var root = "/home/qli/git/ocaml-ci/web"
	var request_button = document.getElementById("request_submit");
	request_button.addEventListener("click", userRequest);


	function userRequest () {
		var ids = sendUserRequest();

		for (var i = 0; i < ids.length; i ++ ) {
			var div = document.getElementById(ids[i]);
			if (div == null)
				createUserRequestDiv(ids[i]); 
		}
	};


	function createUserRequestDiv (id) {
		var jobDiv = document.getElementById("job");

		var newDiv = document.createElement("div");
		newDiv.setAttribute("id", id);
		
		var title = document.createElement("span");
		title.setAttribute("class", "user_request");
		title.innerHTML = "User Request: " + id.slice(0, 5);
		
		var refresh = document.createElement("img");
		refresh.setAttribute("src", root + "/res/pic/refresh.png")
		refresh.addEventListener("click", refreshJob)

		var ulist = document.createElement("ul");

		newDiv.appendChild(refresh);
		newDiv.appendChild(title);
		newDiv.appendChild(ulist);
		jobDiv.appendChild(newDiv);
	};


	function refreshJob () {
		var div = this.parentElement;
		var id = div.getAttribute("id");

		var progresses = sendJobQuery(id);
		var oldUlist = div.children[2];
		div.removeChild(oldUlist);

		var newUlist = document.createElement("ul");
		for (var i = 0; i < progresses.length; i ++) {
			var job = progresses[i];
			var li = document.createElement("li");
			li.setAttribute("class", "job");
			li.setAttribute("id", job.id);
			li.setAttribute("state", job.state);
			li.setAttribute("user_request", job.user);
			li.addEventListener("mouseenter", objectQuery);
			li.addEventListener("mouseleave", cleanObjectQuery);

			var span_id = document.createElement("span");
			span_id.setAttribute("class", "id");
			span_id.innerHTML = job.id.slice(0, 5);

			var span_info = document.createElement("span");
			span_info.setAttribute("class", "info");
			span_info.innerHTML = job.info.slice(6);

			var span_state = document.createElement("span");
			span_state.setAttribute("class", "state");
			span_state.innerHTML = job.state;

			li.appendChild(span_id);
			li.appendChild(span_info);
			li.appendChild(span_state);
			newUlist.appendChild(li);
		}

		div.appendChild(newUlist);
	}


	function objectQuery (event) {
		var state = this.getAttribute("state");
		if (state != "Completed") return;
		else {
			var id = this.getAttribute("id");
			var query = sendObjectQuery(id);
			var inputs = query.inputs;
			for (var i = 0; i < inputs.length; i ++) {
				var id = inputs[i].input_id;
				var all_li = this.parentElement.children;
				for (var j = 0; j < all_li.length; j ++) {
					var li = all_li[j];
					if (li.getAttribute("id") == id)
						li.setAttribute("class", "highlight_job")
				}
			}

			var host = query.host;
			var compiler = query.compiler;
			if(query.result.info)
				var result = query.result.result + " " + query.result.info;
			else var result = query.result.result;

			var temp_span = document.createElement("span");
			temp_span.setAttribute("id", "temp_span");
			
			var id = document.createElement("div");
			id.innerHTML = "Id: " + query.id;
			var h = document.createElement("div");
			h.innerHTML  = "Host: " + host;
			var c = document.createElement("div");
			c.innerHTML  = "Compiler: " + compiler;
			var r = document.createElement("div");
			r.innerHTML  = "Result: " + result;
			
			temp_span.appendChild(id);
			temp_span.appendChild(h);
			temp_span.appendChild(c);
			temp_span.appendChild(r);

			temp_span.style.position = "absolute";
			temp_span.style.top = event.pageY + "px";
			temp_span.style.left = (event.pageX + 15) + "px";

			document.body.appendChild(temp_span);
		}
	}

	function cleanObjectQuery () {
		var span = document.getElementById("temp_span");
		if (span) span.remove();

		var all_li = this.parentElement.children;
		for (var i = 0 ; i < all_li.length; i ++) {
			all_li[i].setAttribute("class", "job")
		}
	}

	function sendUserRequest () {
		var request_form = document.getElementById("request_form");
		var pname = request_form.p_name.value;
		var pversion = request_form.p_version.value;
		var depopt = request_form.depopt.value;
		var compiler = request_form.compiler.value;

		var pin = request_form.pin.value;
		var target = request_form.target.value;
		var repo = request_form.repo.value;
		var addr = request_form.address.value;
		var priority = request_form.priority.value;

		var request = new XMLHttpRequest ();

		var url = base + "package/";
		if (pversion) url += (pname + "." + pversion);
		else url += pname;

		url += "?place=holder"
		if (depopt) url += ("&depopt=" + depopt);
		if (compiler) url += ("&compiler=" + compiler);
		if (pin != "" && target != "")
			url += ("&pin=" + pin + "&target=" + target);
		if (repo != "" && addr != "" && priority != "")
			url += ("&name=" + repo + "&address=" + addr + "&priority=" + priority); 

		//alert(url);
		request.open("POST", url, false);
		request.send();

		return JSON.parse(request.responseText);
	};

	function sendJobQuery(id) {
		var request = new XMLHttpRequest ();
		var url = base + "object/" + id;
		request.open("GET", url, false);
		request.send();
		return JSON.parse(request.responseText);
	};

	function sendObjectQuery(id) {
		var request = new XMLHttpRequest ();
		var url = base + "object/" + id + "/info";
		request.open("GET", url, false);
		request.send();
		return JSON.parse(request.responseText);
	}
};

main ();