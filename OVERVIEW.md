<b>Prerequisite</b> <br/>

Have R installed (I used R studio Version 1.0.136 for Mac)<br/>
Install the libraries in the lib2load.R file <br/>

<b>Exaggerated Income</b> <br/>

Exaggerated income suggests misleading information in the application and should be considered as a risk of fraud and source of mistrust from lenders. <br/>
To flag exaggerated income, I considered groups of applications based on spatial (zip code and state) and economic (annual income, employment title) information. <br/>
I assumed that the annual income was strongly correlated to the employment of the borrower and that the average income per type of job would vary among state. <br/>
It seemed sensible to look at zip code rather than the state with the available amount of data, although I haven’t look at the actual gain of using a refined spatial area. <br/>
To extract the job title, I used a simple method extracting the most frequent word in each employment title. This method has major flaws: misspell, derivate and acronym are ignored, lots of employment should at least be bi-grams (“financial analyst”, “sale manager”). But it showed encouraging distinction in annual income median per job title. <br/>
For each pair of (job, zip) I decided to use the median of the annual income and the standard deviation to define a threshold (2*sd), above which the income should be considered as likely to be exaggerated. I checked the sd, skewness and kurtosis and found that the log of the annual income per area was more normal than the annual income distribution. This should be considered when defining the threshold. <br/>
<br/>
To extract the job title (one word only), I first separated each word of 4 letters or more in each emp_title.</br>
I ordered then by decreasing frequency of appearance. And considered only the first 70 words, representing at least 0.1% of the population. </br>
I noticed two important issues that I could quickly fix manually. </br>
  * First the redundancy of words in the top 70. For instance, assistant and asst.</br> 
  * Second the problem of priority. “assistant” was the immediate spot. I only changed this one. <br/>
This method proved very limiting, giving job titles in the top 70 that aren’t sensible (lead, data) or are too simplistic (manager) and most importantly failing to find a job title for about a third of the application with a emp_title.<br/>
<b>Currently it flags ~3-4% of applications in the stream.</b> </br>
Refining the text mining approach on the job title is a key step in this task. Studying the annual income properties (and correlation with other variables) will help refine the threshold for flagging exaggerated income. </br>


<b>New Geographic Area</b><br/>

Finding new geographic area can be useful for tracking unusual application or close duplicates, both presenting risks of fraud.<br/>
In the case of new application, comparing the annual income to the median in the state can provide a loose cut for potential fraud. 
 <br/>
I looked at application in the stream that had a zip code and a job title with no entry in the training data. <br/>
<b>Currently flagging ~7-8% of applications in aire.stream.data</b><br/>
The next task, aside refining the current definitions of job title and spatial area, would be to take the estimated age of the borrower into account (for instance using the emp_length, the verification_status, the number of accounts). <br/>


<b>Run the code</b><br/>

a.- run the script CommonFile.R. <i>Will load libraries, functions (operations, visualisation) common to both EVENTS </i> <br/>
b.- run the script EXAG_INCOME.R. <i>Will look at all applications and flag those with exaggerated income for a given set (zip, job).</i> <br/>
c.- run the script NEW_GEO_AREA.R. <i>Will look at new application in set (zip, job), flag them and show average income in the corresponding state.</i> <br/>


