var counters = { passed:0, failed:0, 'not-approved':0 };

function reset_counters() {
  counters.passed = counters.failed = counters['not-approved'] = 0;
  $("span.counter").text("0");
}

function increment(id) {
  counters[id]++;
  console.log(counters[id]);
  $("#"+id).text(counters[id]);
  $("#total").text(counters.passed + counters.failed + counters['not-approved']);
}

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
			    $.el.label({class:"show-as"}, "Show as"),
			    $.el.select($.el.option("html"),
					$.el.option("raw html"),
					$.el.option("Prolog DOM"))),
		   $.el.div({class:"input"},
			    $.el.textarea({placeholder:"Enter PlDoc"},
					  test.text||"")),
		   $.el.div({class:"output"},
			    $.el.div({class:"html"}),
			    $.el.div({class:"raw"}),
			    $.el.div({class:"DOM"})),
		   $.el.div({class:"errors"})));
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
	       test.find(".output .html").html(obj.html);
	       test.find(".output .raw").text(obj.html);
	       test.find(".output .DOM").html(obj.dom);
	       if ( obj.result ) {
		 if ( obj.result.dom && obj.result.html ) {
		   test.addClass("passed");
		   increment('passed');
		 } else {
		   increment('failed');
		   test.addClass("failed");
		   if ( !obj.result.dom ) {
		     test.find(".errors")
		         .append($.el.p("Wrong DOM"),
				 $.el.p(obj.approved.dom));
		   }
		   if ( !obj.result.html ) {
		     test.find(".errors")
		         .append($.el.p("Wrong HTML"),
				 $.el.p(obj.approved.html));

		   }
		 }
	       } else {
		 increment('not-approved');
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

function run_all() {
  $(".content").html("");
  reset_counters();

  $.ajax({ url: "/tests",
	   contentType: "application/json; charset=utf-8",
	   success: function(obj) {
	     for(var i=0; i<obj.length; i++)
	       $(".content").append(test(obj[i]));
	     run($("div.test"));
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

  $("div.header").on("click", "input[type=radio]", function(ev) {
    var show = $(ev.target).attr('id').slice(0,-2);
    if ( show == 'total' ) {
      $(".content > div.test").show();
    } else {
      $(".content > div.test").hide();
      $(".content > div.test."+show).show();
    }
  });

  $("#re-run-all").on("click", function() {
    run_all();
  });

  run_all();
});
