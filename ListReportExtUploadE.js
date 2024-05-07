sap.ui.define([
    // "sap/m/MessageToast",
    "sap/ui/export/Spreadsheet",
    "sap/ui/export/library"
],
    function (Spreadsheet, library) {
        'use strict';

        return sap.ui.controller("br.com.redesim.custcheque.ext.controller.ListReportExt", {

            onActionExportExcel: function (oEvent) {

                /* =================================================================== 
                Configura colunas do excel
                =================================================================== */
                var aCols = [];
                var filename = 'Modelo.xlsx'

                aCols.push({
                    label: 'N Contrato Próprio',
                    property: 'Ncontrato',
                    type: library.EdmType.String
                });
                aCols.push({
                    label: 'N Contrato Jurídico',
                    property: 'Ncontratojuridico',
                    type: library.EdmType.String
                });
                aCols.push({
                    label: 'N do Cheque',
                    property: 'Ncheque',
                    type: library.EdmType.String
                });
                aCols.push({
                    label: 'Cliente',
                    property: 'Kunnr',
                    type: library.EdmType.String
                });
                aCols.push({
                    label: 'Data lançamento',
                    property: 'Budat',
                    type: library.EdmType.String
                });
                aCols.push({
                    label: 'Empresa',
                    property: 'Bukrs',
                    type: library.EdmType.String
                });
                aCols.push({
                    label: 'Local de negócio',
                    property: 'Bupla',
                    type: library.EdmType.String
                });
                aCols.push({
                    label: 'Cond. de pagamento',
                    property: 'Zterm',
                    type: library.EdmType.String
                });
                aCols.push({
                    label: 'Número Chamado',
                    property: 'Nchamado',
                    type: library.EdmType.String
                });
                aCols.push({
                    label: 'Valor do cheque',
                    property: 'Valor',
                    type: library.EdmType.String
                });
                aCols.push({
                    label: 'Número CMC7 cheque',
                    property: 'Zcmc7',
                    type: library.EdmType.String
                });

                aCols.push({
                    label: 'Agência do cheque',
                    property: 'Zhktid',
                    type: library.EdmType.String
                });

                aCols.push({
                    label: 'Conta corrente cheque',
                    property: 'Zbankn',
                    type: library.EdmType.String
                });

                aCols.push({
                    label: 'Dígito agência e dígito conta corrente',
                    property: 'Bkont',
                    type: library.EdmType.String
                });

                aCols.push({
                    label: 'Câmara de compensação cheque',
                    property: 'Zcamara',
                    type: library.EdmType.String
                });

                var oSettings = {
                    workbook: { columns: aCols },
                    dataSource: [''],
                    count: 1,
                    fileName: filename,
                };

                var oSheet = new Spreadsheet(oSettings);
                oSheet.build().finally(function () {
                    oSheet.destroy();
                });

            },

            onActionUploadFile: function (oEvent) {
                var that = this;

                /* =================================================================== 
                 Cria Dialogo dinamico (pop-up)
                =================================================================== */
                var oUploadDialog = new sap.m.Dialog({
                    contentWidth: "300px",
                    resizable: true,
                    type: "Message"
                });
                oUploadDialog.setTitle("Carregar arquivo Excel");

                /* =================================================================== 
                Define o servico gateway que ser chamado
                =================================================================== */
                var sServiceUrl = "/sap/opu/odata/sap/ZFI_CUST_CHEQUE_SRV/";
                var oModel = new sap.ui.model.odata.ODataModel(sServiceUrl, false);
                var sSource = sServiceUrl + "UploadFileSet";

                /* =================================================================== 
                Chama Gateway para processar logs
                =================================================================== */

                /* =================================================================== 
                Prepara o responsavel pela carga do arquivo
                =================================================================== */
                var oFileUploader = new sap.ui.unified.FileUploader({
                    width: "100%",
                    fileType: ["xlsx", "xls", "csv"],
                    typeMissmatch: this.handleTypeMissmatch,                    
                });

                /* =================================================================== 
               Define parametros
               =================================================================== */
                oFileUploader.setName("Simple Uploader");
                oFileUploader.setUploadUrl(sSource);
                oFileUploader.setSendXHR(true);
                oFileUploader.setUseMultipart(false);
                oFileUploader.setUploadOnChange(false);

                var oToken = new sap.ui.unified.FileUploaderParameter({
                    name: "x-csrf-token",
                    value: oModel.getSecurityToken()
                });
                oFileUploader.insertHeaderParameter(oToken);

                /* =================================================================== 
                Cria botao para a carga do arquivo
                =================================================================== */
                var oTriggerButton = new sap.m.Button({
                    text: 'Upload',
                    type: 'Accept',
                    press: function () {

                        // Verifica se arquivo foi informado
                        var domRef = oFileUploader.getFocusDomRef();
                        var file = domRef.files[0];

                        if (oFileUploader.getValue() === '') {
                            sap.m.MessageToast.show("Favor selecionar um arquivo");
                            return;
                        }

                        var oContentType = new sap.ui.unified.FileUploaderParameter({
                            name: "Content-Type",
                            value: file.type
                        });
                        oFileUploader.insertHeaderParameter(oContentType);

                        oFileUploader.insertHeaderParameter(new sap.ui.unified.FileUploaderParameter({
                            name: "SLUG",
                            value: oFileUploader.getValue()
                        }));

                        var fnFunctionTeste = function (oEvent) {

                            return new Promise(function (showSuccessMessage, showErrorMessage) {


                                oFileUploader.attachUploadComplete(

                                    function (oEvent) {

                                        /* O upload precisa vim pra cá */

                                        // Atualiza tabela do relatorio apos carga
                                        //var sId = that.oView.sId + '--responsiveTable';
                                        //var oTable = that.byId(sId);
                                        //oTable.getModel().refresh(true);

                                        oUploadDialog.close();
                                        oUploadDialog.destroy();

                                        // Verifica o retorno da mensagem
                                        if (oEvent.mParameters.status == '200' ||
                                            oEvent.mParameters.status == '201' ||
                                            oEvent.mParameters.status == '204') {

                                            showSuccessMessage();
                                            //alert("Carga realizada com sucesso.");


                                        }
                                        else {
                                            that.showLog(oEvent.mParameters.responseRaw);
                                            showErrorMessage();
                                        }

                                        that.templateBaseExtension.getExtensionAPI().refreshTable();
                                        //debugger;

                                    }.bind(this)
                                );

                                oFileUploader.upload();

                            }.bind(this))

                        }.bind(this)

                        var mParameters = {
                            sActionLabel: 'Upload de Arquivo',
                            dataloss: false
                        };


                        this.extensionAPI.securedExecution(fnFunctionTeste, mParameters);


                    }.bind(this)
                });
                /* =================================================================== 
                Cria bot o de cancelar
                =================================================================== */
                var oCancelButton = new sap.m.Button({
                    text: 'Cancelar',
                    type: 'Reject',

                    press: function () {

                        oUploadDialog.close();
                        oUploadDialog.destroy();
                        this.getView().removeAllDependents();

                    }.bind(this)
                });
                /* =================================================================== 
                Chama dialogo com a logica de carga 
                =================================================================== */
                oUploadDialog.addContent(oFileUploader);
                oUploadDialog.setBeginButton(oCancelButton);
                oUploadDialog.setEndButton(oTriggerButton);
                oUploadDialog.open();
            },

            handleTypeMissmatch: function (oEvent) {
                var aFileTypes = oEvent.getSource().getFileType();
                jQuery.each(aFileTypes, function (key, value) { aFileTypes[key] = "*." + value; });
                var sSupportedFileTypes = aFileTypes.join(", ");
                sap.m.MessageToast.show("Tipo de arquivo *." + oEvent.getParameter("fileType") +
                    " nao ? suportado. Escolha um dos tipos a seguir: " +
                    sSupportedFileTypes);
            },

            showLog: function (sMessage) {

                // Recupera texto da mensagem
                let aMessage = [];
                for (const res of sMessage.matchAll(/(?:<message>(.*?)<[/]message>)/gm)) {
                    aMessage.push(res[1]);
                }

                // Recupera o tipo da mensagem
                let aSeverity = [];
                for (const res of sMessage.matchAll(/(?:<severity>(.*?)<[/]severity>)/gm)) {
                    switch (res[1]) {
                        case "error":
                            aSeverity.push(sap.ui.core.MessageType.Error);
                            break;
                        case "info":
                            aSeverity.push(sap.ui.core.MessageType.Information);
                            break;
                        case "success":
                            aSeverity.push(sap.ui.core.MessageType.Success);
                            break;
                        case "warning":
                            aSeverity.push(sap.ui.core.MessageType.Warning);
                            break;
                        default:
                            aSeverity.push(sap.ui.core.MessageType.None);
                            break;
                    }
                }

                // Exibe mensagem
                for (let i = 0; i < aMessage.length; i++) {

                    if (aMessage[i] == "Ocorreu uma exceção") {
                        continue;
                    }

                    //alert(aMessage[i]);

                    //MessageManager.addMessages(new sap.ui.core.message.Message({
                    sap.ui.getCore().getMessageManager().addMessages(new sap.ui.core.message.Message({
                        message: aMessage[i],
                        persistent: true, // create message as transition message
                        type: aSeverity[i]
                    }));
                }
            }

        });
    },

);
