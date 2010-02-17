# TODO: Add comment
# 
# Author: WerftWiebke
###############################################################################


Hey guys, 

here comes the Tarone-holm preocedure, which Anja Victor seems to call the tarone Hommel procedure (is there a difference?)

Unfortunately, the email is in German and the code has also german inputs. sorry jonathan.

Wiebke
________________________________________
Von: dickhaus@cs.tu-berlin.de [dickhaus@cs.tu-berlin.de]
Gesendet: Mittwoch, 10. Februar 2010 21:17
An: Werft Wiebke
Betreff: [Fwd: Re: Tarone-Holm Prozedur]

------------------------ Ursprüngliche Nachricht -------------------------
		Betreff: Re: Tarone-Holm Prozedur
Von:     anja.victor@Unimedizin-mainz.de
Datum:   Mi, 10.02.2010, 21:13
An:      "Dr. Thorsten Dickhaus" <dickhaus@cs.tu-berlin.de>
		--------------------------------------------------------------------------
		
		ging mit der anderen Mail leider nicht, und unser externen Mailzugang ist
echt
doof hier also in Textform:
		
		
		taronehommelneu=function(pwerte, minniv,ngen,alpha)
{
	verwerfth=NA;
	verworfene=rep(0, times=ngen);
	ka=NA;
	modalitaet=NA;
	
	sortedminniv=sort(minniv)
	sortedindexminniv=sort(minniv,index.return=T)$ix;
	
	for  (r in 1:(ngen-1)) {if (((r-1)*sortedminniv[r]<=alpha) &
				(alpha<r*sortedminniv[r+1]))
		{rn=0;break} else{rn=1}
	}
	ka=r;
	if (rn==1) {ka=ngen};
	sortedp=sort(pwerte)
	sortedindexp=sort(pwerte,index.return=T)$ix;
	if (sortedminniv[ka]>(alpha/ka))
	{ modalitaet=1} else{modalitaet=0}
	for (j in 1:ngen) {
		if ((sortedp[j]>(alpha/ka))&(sortedp[j]>=sortedminniv[ka])) {break}
	}
	verwerfth=j-1
	if (verwerfth>0){verworfene[sortedindexp[1:verwerfth]]=rep(1,
				times=verwerfth)}
	
	ausgabe=list(modalitaet=modalitaet,ka=ka, verwerfth=verwerfth,
			verworfene=verworfene)
	return(ausgabe)
	
}


ptest=c(0.217,0.268,0.669,0.0058,0.291,0.712,0.712,0.665, 0.818)
minnivtest=c(0.026, 0.0039, 0.035, 0.00019, 0.043, 0.041, 0.041, 0.029,
		0.007)


taronehommelneu(ptest,minnivtest,9,0.05)

ist nix gropßartiges


Quoting "Dr. Thorsten Dickhaus" <dickhaus@cs.tu-berlin.de>:
		
		> Liebe Anja!
		>
		> Wir sind mit der Programmierung des µTOSS Systems jetzt schon so weit
> fortgeschritten, dass wir auch externe Methoden Dritter andocken
> können.
>
		> Als Probe aufs Exempel und natürlich auch aus wissenschaftlichem
> Interesse möchten wir Dich gerne fragen, ob Du uns Deinen Code für
> die Tarone-Hom Prozedur (gegen Copyright-Vermerk natürlich!)
> übersenden könntest, damit er Teil von µTOSS wird.
>
		>
		>
		> Danke im Voraus,
> Thorsten.
>
		> --
		> ******************************************************
		> * Dr. rer. nat. Thorsten Dickhaus                    *
		> * Berlin Institute of Technology                     *
		> * Machine Learning / Intelligent Data Analysis Group *
		> * Sekr. FR6-9                                        *
		> * Franklinstrasse 28/29, Room 7026                   *
		> * D-10587 Berlin                                     *
		> * Tel.: +4930/314-28678                              *
		> * Fax : +4930/314-78622                              *
		> * E-Mail  : dickhaus@cs.tu-berlin.de                 *
		> * Homepage: http://cs.tu-berlin.de/~dickhaus         *
		> ******************************************************
		>
		>
		
		
		
		
		
