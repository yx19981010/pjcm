package com.samsph.pjcm.controller;

import com.samsph.pjcm.config.constant.FileType;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.service.CertificateService;
import com.samsph.pjcm.service.MaterialService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.tomcat.util.http.fileupload.IOUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import java.io.*;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

import static com.samsph.pjcm.config.constant.ErrMsg.POST_FILE_NOT_EXISTS;
import static com.samsph.pjcm.config.constant.ErrMsg.POST_FILE_READ_ERROR;

@Api(tags = "文件下载")
@Validated
@RestController
@Slf4j
@RequestMapping(value = "/api/v1")
public class FileDownloadController {
    @Autowired
    private CertificateService certificateService;
    @Autowired
    private MaterialService materialService;

    /**
     * 下载（证书1、材料2）
     * @param id    文件id
     * @param type  文件类型
     * @param response
     * @param request
     */
    @ApiOperation(value = "下载证书，材料")
    @ApiImplicitParams({
            @ApiImplicitParam(name="id",value="文件id"),
            @ApiImplicitParam(name="type",value="文件类型")
    })
    @GetMapping(value = "/fileDownload/id={id}&type={type}")
    public void FileDownload(@NotNull(message = "id不能为空")@Min (value = 1,message = "文件id必须是正整数")@PathVariable(value = "id") Integer id,
                             @NotNull(message = "type不能为空")@Min (value = 1,message = "类型最小值为1")@Max (value = 2,message = "类型最大值为2")@PathVariable(value = "type") Integer type,
                             HttpServletResponse response,
                             HttpServletRequest request) {
        String filePath;
        switch(type){
            case FileType.CERTIFICATE: //证书
                if(certificateService.findCertificate(id).isPresent()) {
                    filePath = certificateService.findCertificate(id).get().getPath();
                    if(filePath == null || StringUtils.isBlank(filePath)){
                        throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "证书不存在");
                    }
                    download(filePath, response, request,certificateService.findCertificate(id).get().getName());
                }else{
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "证书id无效或未输入证书id!!!!");
                }
                break;
            case FileType.MATERIAL://资料
                if(materialService.findMaterial(id).isPresent()) {
                    filePath = materialService.findMaterial(id).get().getPath();
                    if(filePath == null || StringUtils.isBlank(filePath)){
                        throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "材料不存在");
                    }
                    download(filePath, response, request,materialService.findMaterial(id).get().getName());
                }else{
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "材料id无效或未材料证书id!!!!");
                }
                break;
        }
    }

    /**
     * 文件下载
     * @param pathname
     * @param response
     * @param request
     */
    private void download(String pathname, HttpServletResponse response, HttpServletRequest request,String filename) {
        response.setCharacterEncoding(request.getCharacterEncoding());
        response.setContentType("application/octet-stream");
        FileInputStream fis = null;
        try {
            File file = new File(pathname);
            if (!file.exists()) {
                throw new CustomException(CustomExceptionType.SYSTEM_ERROR, POST_FILE_NOT_EXISTS);
            } else {
                fis = new FileInputStream(file);
                response.setHeader("Content-Disposition",
                        "attachment; filename=" + URLEncoder.encode(filename+file.getName().substring(file.getName().lastIndexOf(".")), String.valueOf(StandardCharsets.UTF_8)));
                IOUtils.copy(fis, response.getOutputStream());
                response.flushBuffer();
            }
        } catch (IOException ex) {
            throw new CustomException(CustomExceptionType.SYSTEM_ERROR, POST_FILE_READ_ERROR);
        } finally {
            if (fis != null) {
                try {
                    fis.close();
                } catch (IOException ex) {
                    log.error(Arrays.toString(ex.getStackTrace()));
                }
            }
        }
    }
}

