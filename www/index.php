
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<p>&#956;TOSS (multiple hypotheses testing in an open software system) aims at providing a unified, extensible interface covering a large spectrum of multiple hypothesis testing procedures in R.<br>
It features a graphical user interface and a simulation tool.<br>

<table border=0  bgcolor=#FFFFFF align=center>
<tr>
<td><a href="./PASCAL2.png"><img src="./PASCAL2.png" align="right" height="165" width="517"></a></td>
</tr>
</table>

<h3 align=center>General project information</h2>

<table border=1  bgcolor=#FFFFFF align=center>
  <tr>
     <td>Coordinators</td>
     <td>Dr. Thorsten Dickhaus, Berlin Institute of Technology (dickhaus@cs.tu-berlin.de)<br>
Dr. Gilles Blanchard, Weierstrass Institute for Applied Analysis and Stochastics (WIAS) Berlin (blanchar@wias-berlin.de)<br>
Prof. Dr. Klaus-Robert M&uuml;ller, Berlin Institute of Technology</td> 
 </tr>
  <tr>
     <td>Participants</td>
     <td>
     Niklas Hack, Medical University of Vienna<br>
     Dr. Frank Konietschke, Georg-August-University G&ouml;ttingen<br>
     Kornelius Rohmeyer, Leibniz University Hannover<br>
     Jonathan Rosenblatt, Tel Aviv University<br>
     Marsel Scheer, German Diabetes Center D&uuml;sseldorf<br>
     Wiebke Werft, German Cancer Research Center Heidelberg</td>
  </tr>
  <tr>
     <td>Hosting site</td>
     <td>Berlin Institute of Technology, Faculty IV, Machine Learning Department</td>
  </tr>
  <tr>
     <td>Coding period</td>
     <td>January 18th - February 12th, 2010</td>
  </tr>
<tr>
     <td>Project Workshop</td>
     <td>February 15th - February 16th, see <a href="http://amiando.com/mutoss.html" target="_blank">Workshop website</a></td>
  </tr>
</table>

<h3>Further Reading</h2>
<ul>
<LI>Official &#956;TOSS Website: <a href="http://www.mutoss.eu" target="_blank">www.mutoss.eu</a></LI>
<LI>Wishlist for requesting methods and data types: see <a href="http://mutoss.eu/mutoss-wishlist.html" target="_blank">here.</a><br>
<b>If you have further wishes, you are most welcome to address them to Thorsten Dickhaus via email.</b></LI>
<LI>Website of the <a href="http://amiando.com/mutoss.html" target="_blank">&#956;TOSS Workshop</a></LI>
</ul>
</p> 

<!-- <?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?> -->

<!-- end of project description -->

<p>The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>.</p>

<h3>Features:</h3>
<h4>Unified interface</h4>
<h4>Repository</h4>

</body>
</html>
