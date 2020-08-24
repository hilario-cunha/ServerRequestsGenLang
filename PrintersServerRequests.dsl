u [Tlantic.Functional]
f Printers
m TryToGetPrinterRequest PrinterEntryReponse [StringNotEmpty macAddress]
printers/{macAddress} 
m TryToGetPrintersRequest [PrinterEntryReponse] [String retailStoreId]
printers?store={retailStoreId}
