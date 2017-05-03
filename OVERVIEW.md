* prerequisite to run your code <br/>
Have R installed (I used R studio Version 1.0.136 for Mac)<br/>
Install the libraries in the lib2load.R file <br/>
<br/>

<b>1.- Exaggerated Income</b><br/>
I choose to look at exaggerated income per type of job and per zip/state area. </br>
I used zip area as it was more refined than state. For each 3-digit zip code and job title I used the median and the sigma to define my threshod:</br>
threshold <- med_salary+(2*sd_salary)</br>
I only considered pair (zip, job) with 20 entries of more. </br>
To extract the job title, I first separated each word of 4 letters or more in each emp_title.</br>
I ordered then by decreasing frequency of appearance. And considered only the first 70 words, with at least 0.1% of the population. </br>
I should extend that but there is too many flaws to improve first.</br>
<br/>
I noticed two important issues that I could quickly fix manually. </br>
  * First the redundancy of words in the top 70. For instance assistant and asst.</br> 
  * Second the problem of priority. Manager assistant was the immediate spot. I only changed this one.
<br/>
N.B: I saw a lot of jobs in the top 70 that don't seem sensible (For instance lead, data). </br>
The job title needs to be refined. Title with acronym, derive and misspel needs to be treated.</br>
Ngram are to be used. There is statistics to do so and a tangible difference to expect (analyst vs financial analyst; account manager vs store manager)</br>
<b>Currently flagging ~3-4% of applications in aire.stream.data</b> </br>
</br>


<b>2.- New Geographic Area</b><br/>
For the new geographic area, I decided to explore zip as I thought that some state may have particular area not yet active. <br/>
And therefore could be interesting for marketing purposes or for flagging potential risk feature. <br/>
I looked at zip codes online but having only three digits and the state in the data limits the potential of these.<br/>
I flagged applications from a new geographic area when a new zip is hit for a specific job.<br/>
If available the median of the annual income for a particular job computed in the state is given. <br/>
I need to investigate the annual income distribution itself, consider tranches, and problably use its log value for normality. <br/>
<b>Currently flagging ~7-8% of applications in aire.stream.data</b><br/>

<b>3.- Run the code</b><br/>
a.- run the script CommonFile.R. <i>Will load libraries, functions (operations, visualisation) common to both EVENTS </i> <br/>
b.- run the script EXAG_INCOME.R. <i>Will look at all application and flag those with exaggerated income for a given set (zip, job).</i> <br/>
c.- run the script NEW_GEO_AREA.R. <i>Will look at new application in set (zip, job), flag them and show average income in State.</i> <br/>



