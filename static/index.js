async function getJson(url) {
    try {
        const response = await fetch(url, {method: "GET"});
        if (!response.ok) {
            console.log(error);
        }
        const data = await response.json();
        return data;
    } catch (error) {
        console.log(error);
    }
}

async function getTopics() {
    return await getJson("/topics");
}

function collectTopics() {
    const checkboxes = document.getElementsByName("topics");
    const indices = Array.prototype.slice.call(checkboxes)
        .filter(ch => ch.checked == true)
        .map(t => t.value)
        .join(",");
	localStorage.setItem("topics", JSON.stringify(indices));
    return indices;
}

async function getSituation(topics) {
    return await getJson("/situation?topics=" + topics);
}

// Create checkbox options for each topic the server supports.
getTopics().then(topics => {
	const saved_checkboxes = JSON.parse(localStorage.getItem("topics"));
    const topic_section = document.getElementById("topic_list");
    topics.forEach(topic => {
        const tag_name = "topic_" + topic.index;
        const checkbox = document.createElement("input");
        checkbox.type  = "checkbox"
        checkbox.id    = tag_name;
        checkbox.name  = "topics";
        checkbox.value = topic.index;
		if (saved_checkboxes) {
			checkbox.checked = saved_checkboxes.includes(topic.index);
		} else {
			checkbox.checked = topic.select_by_default;
		}

        const topic_name = document.createElement("span");
        topic_name.innerHTML = topic.name;

        // Make the text of the checkbox clickable, too.
        const label = document.createElement("label");
        label.appendChild(checkbox);
        label.appendChild(topic_name);

        newline = document.createElement("p");
        newline.classList.add("topic");
        newline.appendChild(label);
        topic_section.appendChild(newline);
    })

    setWidth("dlrvul", "Dealer: W  ");
    // I don't know the widths of the suit symbols; substitute a W.
    setWidth("top_col1", "W  A K Q J 10 9 8 7");
    setWidth("top_col2", "W  A K Q J 10 9 8 7");
    setWidth("top_col3", "W  A K Q J 10 9 8 7");

    const current_topics = collectTopics();
    getSituation(current_topics).then(displayProblem);
    cacheProblem(current_topics);
})

var footnote_value;

function makeBiddingRow(calls, show_our_alerts) {
    alert_section = document.getElementById("alerts");
    row = document.createElement("tr");
    var is_opps_call = true; // First call is by West

    calls.forEach(call => {
        item = document.createElement("td");
        item.style = "text-align: left; padding-left: 0.5em; padding-right: 0.5em;";
        item.innerHTML = call.call ?? "";
        item.width = "25%";

        if (call.alert !== undefined && (is_opps_call || show_our_alerts)) {
            sup = document.createElement("sup");
            sup.innerHTML = String.fromCharCode(footnote_value);
            item.appendChild(sup);

            description = document.createElement("li");
            description.innerHTML = call.alert;

            footnote_value += 1;
            alert_section.appendChild(description);
        }
        row.appendChild(item);
        is_opps_call = !is_opps_call;
    })

    return row
}

function displayBiddingHeaders(table) {
    tr = document.createElement("tr");
    for (const direction of ["West", "North", "East", "South"]) {
        item = document.createElement("th");
        item.style = "text-align: left; padding-left: 0.5em; padding-right: 0.5em;";
        item.innerHTML = direction;
        item.width = "25%";
        tr.appendChild(item);
    }
    table.appendChild(tr);
}

function displayBidding(bids, show_our_alerts) {
    footnote_value = 97;  // lower-case 'a'
    clear("alerts");

    table = document.createElement("table");
    table.style = "table-layout:fixed;";
    displayBiddingHeaders(table);

    bids.forEach(round => {
        table.appendChild(makeBiddingRow(round, show_our_alerts));
    })

    bidding = clear("bidding");
    bidding.appendChild(table);
}

function setValue(id, val) {
    elem = document.getElementById(id);
    elem.innerHTML = val;
}

// Removes all child nodes and returns the current node.
function clear(id) {
    elem = document.getElementById(id);
    while (elem.firstChild) {
        elem.removeChild(elem.firstChild);
    }
    return elem;
}

var current_problem = null;
var next_problem = null;
var topics_for_next_problem = "";

async function cacheProblem(topics) {
    getSituation(topics).then(problem => {
        console.log("caching next problem...");
        next_problem = problem;
        topics_for_next_problem = topics;
    });
}


async function nextProblem() {
    // If the user has changed which topics are checked, throw out the cached
    // problem.
    current_topics = collectTopics();
    if (topics_for_next_problem !== current_topics) {
        next_problem = null;
    }

    // If we have a cached problem, use that.
    if (next_problem != null) {
        console.log("using cached problem...");
        displayProblem(next_problem);
    } else {
        console.log("getting new problem in real time...");
        getSituation(current_topics).then(displayProblem);
    }

    // Get ready for next time.
    cacheProblem(current_topics);
}


async function displayProblem(problem) {
    current_problem = problem;
    console.log(problem);
    displayBidding(problem.bidding, false);
    setValue("dealer", problem.deal.dealer);
    setValue("vulnerability", problem.deal.vulnerability);
    displayHand(problem.deal.south_hand, "south_hand");
    clear("west_hand");
    clear("east_hand");
    clear("north_hand");

    show_ans = document.createElement("button");
    // The mobile version needs to set the text size of the button larger than
    // default. So, give it a class that can be modified by mobile.css.
    show_ans.className = "button";
    show_ans.innerHTML = "Show Answer";
    show_ans.onclick = displaySolution;
    expl = clear("explanation");
    expl.appendChild(show_ans);
    expl.style = "";
}

function displayHand(hand, id) {
    function displayHolding(suit, holding) {
        return "<tr><td><center>" + suit + "</center></td><td>" +
            holding + "</td></tr>";
    }

    function red(symbol) {
        return "<span style='color: red'>" + symbol + "</span>";
    }

    elem = document.getElementById(id);
    elem.innerHTML = (
        "<table style='border-spacing: 0; border-collapse: collapse;'>" +
        displayHolding(    "&spades;",  hand.spades) +
        displayHolding(red("&hearts;"), hand.hearts) +
        displayHolding(red("&diams;"),  hand.diamonds) +
        displayHolding(    "&clubs;",   hand.clubs) +
        "</table>");
    elem.style = "word-spacing:-0.1em;"
}

function displaySolution() {
    displayHand(current_problem.deal.west_hand, "west_hand");
    displayHand(current_problem.deal.east_hand, "east_hand");
    displayHand(current_problem.deal.north_hand, "north_hand");
    displayBidding(current_problem.bidding, true);

    expl = clear("explanation");
    expl.style="vertical-align:top;";
    ans = document.createElement("p");
    ans.innerHTML = "Answer: " + current_problem.answer;
    expl.appendChild(ans);
    sol = document.createElement("p");
    sol.innerHTML = current_problem.explanation;
    expl.appendChild(sol);
    expl.appendChild(document.createElement("hr"));
    dbg = document.createElement("p")
    dbg.innerHTML = "debug string: " + current_problem.debug_string;
    expl.appendChild(dbg);
}

function setWidth(elemId, text) {
    elem = document.getElementById(elemId);

    // Solution for computing width is inspired by
    // https://stackoverflow.com/a/21015393
    const canvas = document.createElement("canvas");
    const context = canvas.getContext("2d");
    // Solution for getting default font is inspired by
    // https://stackoverflow.com/a/7444724
    context.font = window.getComputedStyle(elem, null).font;
    const metrics = context.measureText(text);

    elem.style.width = metrics.width + "px";
    elem.style.minWidth = metrics.width + "px";
    console.log(metrics.width);
}
