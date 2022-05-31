COLUMN DATA

Fixed Acidity- acids that do not evapoarate easily; found naturally in grapes or occur during fermentation
    
    Fixed Acids: tartaric, citric, malic (occur naturally in grapes), succinic (fermentation)
    
    Expressed in Dataset: as tartaric acid
    
    Dataset Units: g/dm3
    
    Industry Standard Units (re-coded conversion): mg/L 
    
    Outlier Note: NO OUTLIER THRESHOLD
        While there are general ranges often seen in wines for each of the fixed acids, because they are naturally occuring, they are dependent on the grapes themselves (different amounts in different types of grapes/grape crops)

    https://waterhouse.ucdavis.edu/whats-in-wine/fixed-acidity#:~:text=The%20predominant%20fixed%20acids%20found,2%2C000%20mg%2FL%20succinic%20acid

Volatile Acidity-

    Volatile Acids: steam distillable acids in wine: primarily acetic acid, also lactic, formic, butyric, and propionic acids
    
    Expressed in Dataset: as acetic acid
    
    Dataset Units: g/dm3
    
    Industry Standard Units (re-coded conversion): mg/L 

    Notes: Acetic acid  <400 mg/L in a new,dry table wine, typical levels from undetectable to 3g/L.
        Aroma detection of acetic acid in red wine from 600 mg/L and 900 mg/L. 

    Outlier Note: Thresholds can go up to 3 g/L (3000 mg), but legal limit for wine labeling/sale is 1.2 g (1200 mg).  Anything above 1200 mg would be a true outlier because it could not be sold.  For analysis, we may want to look at how the quality begins to be affected at aroma detection from 600-900 mg/L and then from the 900-1200 mg/L range.

    https://waterhouse.ucdavis.edu/whats-in-wine/volatile-acidity


Citric Acid- weak organic acid (fixed acid)

    Properties: antimicrobial activity on molds and bacteria; relationship with antioxidants by chelating metal ions to prevent browning.

    Relationship to Volatile Acid: citric-sugar co-metabolism can also increase the formation of volatile acid in wine which can affect the wine aroma negatively if present at excessive levels.
   
    Dataset Units: g/dm3
    
    Industry Standard Units (re-coded conversion): mg/L 

    Outlier Note: fixed acid---NO OUTLIER THRESHOLD

    https://wineserver.ucdavis.edu/industry-info/enology/methods-and-techniques/common-chemical-reagents/citric-acid


Residual Sugar- The sugars left unfermented in a finished wine (measured in grams per liter)
    
    Dataset units: g/dm3 (no conversion needed)

    Labeling terms, still wines in the EU:
    Up to 4 g/l---      Dry
    4 g/l – 12 g/l---   Medium dry 
    12 g/l – 45 g/l---  Medium (Medium sweet)
    More than 45 g/l--- Sweet 
  
    https://www.decanter.com/learn/residual-sugar-46007/


Chlorides- Amount of sodium choride present in wine (mg/L).Generally relatable to the salinity of the soil.
    
    Expressed in Dataset: as sodium chloride
    
    Dataset units: g/dm3

    Industry Standard Units (re-coded conversion): mg/L 

    Affects on quality:  
        Detectable in red wine: 520 mg/L
        Recognition in red wine: 2980 mg/L

    Outliers: Still seeing if I can find some more background.  Argentina is noted as having the most stringent guidelines at 600 mg/L.  Australia allows for 1000 mg/L.

    With the highest in our dataset being at 611 mg/L we probably do not have any outliers here.  

    https://www.awri.com.au/wp-content/uploads/2018/08/s1530.pdf#:~:text=The%20higher%20extraction%20of%20chloride,juice%20x%201.7%20%3D%20606)


Free Sulfur Dioxide- free form of SO2; exists in equilibrium between molecular SO2 (as a dissolved gas) and bisulfite ion

    Dataset units: ppm (no conversion necessary)

    FS02 and the pH determine how much sulfur dioxide is available to protect the wine from oxidation.

    Too much FSO2 can be perceptible to consumers, masking the wine’s aromas and inhibiting its ability  breathe. In high enough concentrations, contributing a sharp/metallic/bitter flavor or sensation.

    https://www.extension.iastate.edu/wine/total-sulfur-dioxide-why-it-matters-too/


Total Sulfur Dioxide- The SO2 that is free in the wine + portion that is bound to other chemicals in  wine.

    Dataset units: ppm (no conversion necessary)

    Oultiers: maximum allowable legal amount is 350 ppm; our highest is 289 ppm; NO OTULIERS

    https://www.extension.iastate.edu/wine/total-sulfur-dioxide-why-it-matters-too/#:~:text=The%20TSO2%20level%20is%20also,mg%2FL)%20of%20TSO2



Density: before fermentation the density of wine juice is thicker than water.
    As the yeast consumes the sugar in the wine, and converts it to alcohol, the liquid is less dense. 
    After fermentation, the specific gravity of wine often less than 1.00, usually 0.996.

    Dataset Units: g/dm3 (no conversion needed)


pH- pH: overall ranges on a scale from 0 -14, 0 being the most acidic, 14 being the most basic

    pH level of a wine typically ranges from 3 to 4.

    Color: Red wines with higher acidity >bright ruby color, lower pH gives them a red hue. 
    Higher pH, less-acidic red wines can take on a blue or purple hue or even brown due to oxidation. 
    Oxidation can be a problem regardless of pH, but the reaction occurs quicker at a higher pH.

    Taste: Wine with high acid will usually taste crisper/more tart on the palate. 
    A low-acid wine will feel smoother and rounder on the palate. 
    
    When done carefully, it can be a lovely, but when low acid is combined with high pH, a flabby or almost soapy feeling can be present.

    High acid wines> more likely to improve with time. 

    Stability of high acid/low pH wines helps during aging, but high pH wines are more prone to microbes.
    That is corrected by adding S02. Takes a lot more sulfur dioxide to get the same effect in a wine at pH level of 4 as one with 10 times more acidity at pH level of 3.

    Wines with higher pH require more sulfites to protect them from oxidation.


Sulphates- Looking into more info on this, specific to the compound 
    Expressed in dataset as: potassium sulphate

    Dataset Units: g/dm3

    Industry Standard Units (re-coded conversion): mg/L 

    By adding potassium metabisulfite after you've stopped fermentation completely you can then back sweeten a wine with little risk of rekindling the fermentation of newly added sugar.



Alcohol- Measured by recording (starting specific gravity - final specific gravity) * 131

