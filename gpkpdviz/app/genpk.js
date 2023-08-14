/* Functions */

function tabBadge(tabName, tabStatus, tabBadgeIcon = 'check', tabBadgeText = '', addnlTabBadgeClass = '') {

    var tabId = tabName + '-tabBadge';

    // Remove Old Badge
    $('#' + tabId).remove();

    if (tabStatus == 'danger') {
        tabBadgeIcon = 'close';
    }

    if (tabStatus == 'initial') {
        tabBadgeIcon = 'fw';
    }

    var tabBadgeClassElement = "pull-right badge alert-" + tabStatus + addnlTabBadgeClass;

    var badgeStyle = '';

    if (tabBadgeText.length > 0) {
        badgeStyle = "style = 'font-family:Sans-Serif' ";
    }

    var tabBadgeIconElement = "<i " + badgeStyle + "class='fa fa-" + tabBadgeIcon + "'>" + tabBadgeText + "</i>";

    var newBadge = jQuery('<div/>', {
        id: tabId,
        class: tabBadgeClassElement,
        html: tabBadgeIconElement
    });

    $(".sidebar-menu").find("span:contains(" + tabName + ")").last().append(newBadge);
}

function boxClass(boxId, newClass) {

    var parentbox = $('#' + boxId).closest('.box');

    parentbox.removeClass('box-primary box-success box-info box-warning box-danger');

    if (newClass != 'none') {
        parentbox.addClass('box-' + newClass);
    }
}

/*
function modelLoadSuccess(modelName) {

    boxClass('model-load-box', 'success');

    $('#model-load-message').empty().html("Model <i>" + modelName + "</i> Loaded").fadeIn('slow', function () {
        $('#model-build').show();
    });
}
*/


function modelLoadChange(event, modelName) {
   boxClass('model-load-box', event);

   if (event == 'success') {
    $('#model-load-message').empty().html("Model <i>" + modelName + "</i> Loaded").fadeIn('slow', function () {
        $('#model-build').show();
    });
   } else {
         $('#model-load-message').empty().fadeOut('fast', function () {
        $('#model-build').hide();
    });
   }
}



function modelBuildChange(event) {

    var boxToChange = ['model-summary', 'model-summary-box', 'model-parameters'];
    boxClass('model-build-box', event);

    if (event == 'success') {

        $('#model-build-message').fadeIn('slow', function () {
            for (i = 0; i < boxToChange.length; i++) {
                $('#' + boxToChange[i]).css("visibility", "visible");
            }
        });

    } else {
        $('#model-build-message').fadeOut('fast', function () {
            for (i = 0; i < boxToChange.length; i++) {
                $('#' + boxToChange[i]).css("visibility", "hidden");
            }
        });
    }


}
/*
function modelBuildSuccess() {

    boxClass('model-build-box', 'success');

    $('#model-build-message').fadeIn('slow', function () {
        $('#model-summary').css("visibility", "visible");
        $('#model-summary-box').css("visibility", "visible");
        $('#model-parameters').css("visibility", "visible");
    });
}
*/
function modelBuildFail() {

    boxClass('model-build-box', 'danger');

    $('#model-build-message').hide();
    $('#model-summary').css("visibility", "visible");
}



function simIDValidation(response) {
    if (response == 'success') {
        $('#sim_id').removeClass('parsley-error').addClass('parsley-success');
    } else {
        $('#sim_id').removeClass('parsley-success').addClass('parsley-error');
    }

}


function removeSimTab() {
    $('a[href="#shiny-tab-simulation"]').parent().remove();
}


function simulationTabBadge(nSims) {

    var tabId = 'Simulations' + '-tabBadge';
    // Remove Old Badge
    $('#' + tabId).remove();


    var tabBadgeIcon = 'fw';


    var newBadge = jQuery('<div/>', {
        id: tabId,
        class: "pull-right badge alert-" + "success",
        html: "<i class='fa fa-" + tabBadgeIcon + "'>" + nSims + "</i>"
    });

    $(".sidebar-menu").find("span:contains(" + 'Simulation' + ")").append(newBadge);
}
/*
function beforeModelLoad() {
    boxClass('model-load-box', 'primary');
    var boxToHide = ['model-load-message', 'model-build', 'model-build-message'];

    for (i = 0; i < boxToHide.length; i++) {
        $('#' + boxToHide[i]).css('display', 'none');
    }

    $('#model-summary-box').css('visibility', 'hidden')
}
*/

function closeCovarBox(){
  $("h3:contains('Covariate Specifications')").parent().parent().find(".fa-minus").click();
}

$(document).ready(function () {
    Shiny.addCustomMessageHandler("genpkJS",
        function (code) {
            eval(code);
        });
});
