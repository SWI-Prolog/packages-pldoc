function adjust_textarea(ta) {
  var lines = ta.val().split("\n").length;
  var h = parseFloat(ta.css("line-height"))*lines;
  ta.height(h);
}

function test_id() {
  for(var i=1; ; i++)
  { var id = "t"+i;

    if ( $("#"+id).length == 0 )
      return id;
  }
}


function test(test) {
  var div;

  test = test||{};

  $(".content").append(
    div = $.el.div({class:"test show-html"},
		   $.el.div({class:"control"},
			    $.el.input({value:test.name||test_id()}),
			    $.el.button({class:"approve"}, "Approve"),
			    $.el.button({class:"run"}, "Re-run"),
			    $.el.label("Show as"),
			    $.el.select($.el.option("html"),
					$.el.option("raw html"),
					$.el.option("Prolog DOM"))),
		   $.el.div({class:"input"},
			    $.el.textarea({placeholder:"Enter PlDoc"},
					  test.text||"")),
		   $.el.div({class:"output"},
			    $.el.div({class:"html"}),
			    $.el.div({class:"raw"}),
			    $.el.div({class:"DOM"}))));
  div = $(div);

  adjust_textarea(div.find("textarea"));
  if ( test.name ) {
    div.attr("id", test.name);
    div.find("input").attr("readonly", "readonly");
  }

  return div;
}

function run(tests) {
  return tests.each(function() {
    var test = $(this);
    var text = test.find(".input textarea").val();
    var name = test.find(".control input").val();

    test.removeClass("passed failed not-approved");

    $.ajax({ type: "POST",
	     url: "/wiki",
	     contentType: "application/json; charset=utf-8",
	     dataType: "json",
	     data: JSON.stringify({ name: name, text: text}),
	     success: function(obj) {
	       console.log(obj);
	       test.find(".output .html").html(obj.html);
	       test.find(".output .raw").text(obj.html);
	       test.find(".output .DOM").html(obj.dom);
	       if ( obj.result ) {
		 console.log(obj.result);
		 if ( obj.result.dom && obj.result.html )
		   test.addClass("passed");
		 else
		   test.addClass("failed");
	       } else {
		 test.addClass("not-approved");
	       }
	     }
    });
  });
}

function show(test, how) {
  test.removeClass("show-html show-raw show-DOM");
  switch(how) {
    case "html":
      test.addClass("show-html");
      break;
    case "raw html":
      test.addClass("show-raw");
      break;
    case "Prolog DOM":
      test.addClass("show-DOM");
      break;
  }
}


function approve(test) {
  var text = test.find(".input textarea").val();
  var name = test.find(".control input").val();

  $.ajax({ type: "POST",
	   url: "/approve",
	   contentType: "application/json; charset=utf-8",
	   dataType: "json",
	   data: JSON.stringify({ name: name, text: text}),
	   success: function(obj) {
	     run(test);
	   }
  });
}

		 /*******************************
		 *	  INITITIALISE		*
		 *******************************/

$(function () {
  $("#new").on("click", function() {
    $(".content").append(test());
  });

  $.ajax({ url: "/tests",
	   contentType: "application/json; charset=utf-8",
	   success: function(obj) {
	     for(var i=0; i<obj.length; i++)
	       $(".content").append(test(obj[i]));
	     run($("div.test"));
	   }
  });

  $("body").on("click", "div.control button", function(ev) {
    var btn  = $(ev.target).closest("button");
    var test = $(btn).closest(".test");

    if ( btn.hasClass("run") ) {
      run(test);
    } else if ( btn.hasClass("approve") ) {
      approve(test);
    }
  });

  $("body").on("click", "div.control select", function(ev) {
    var opt = $(ev.target).closest("option");
    var test = $(opt).closest(".test");
    show(test, opt.text());
  });

  $("body").on("keyup", "textarea", function(ev) {
    adjust_textarea($(ev.target));
    run($(ev.target).closest("div.test"));
  });
});
