package com.samsph.pjcm.web.controller;

import com.samsph.pjcm.config.auth.CurrentUser;
import com.samsph.pjcm.config.PageData;
import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.config.constant.FileUploadPath;
import com.samsph.pjcm.model.Material;
import com.samsph.pjcm.service.MaterialService;
import com.samsph.pjcm.config.utils.DozerUtil;
import com.samsph.pjcm.config.utils.FileUtil;
import com.samsph.pjcm.vo.MaterialVoGet;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.sql.Date;
import java.util.List;

@Api(tags = "材料管理")
@Validated
@RestController
@RequestMapping(value = "/api/v1")
public class MaterialController {
    @Autowired
    private MaterialService materialService;

    @ApiOperation(value = "添加材料")
    @ApiImplicitParams({
            @ApiImplicitParam(name="name",value="材料名"),
            @ApiImplicitParam(name="content",value="材料描述"),
            @ApiImplicitParam(name="filename",value="材料文件")
    })
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    @PostMapping("/materials")
    public AjaxResponse addMaterial(@Size(min = 1,max = 50) @RequestParam("name") String name,
                                    @Size(min = 1,max = 100) @RequestParam(name = "content",required = false) String content,
                                    @RequestParam("filename") MultipartFile file) {
        //权限检测 用户身份是管理员且登录
        String pathname = FileUtil.FileUpload(FileUploadPath.MaterialFileUploadPath, file);
        Date time = new Date(new java.util.Date().getTime());
        Material material = new Material();
        material.setContent(content);
        //定义为登录后管理员的id
//        material.setCreateByUid(currentUser.getCurrentUser().getUserId());
        material.setCreateByUid(7);
        material.setCreateTime(time);
        material.setName(name);
        material.setPath(pathname);
        materialService.addMaterial(material);
        return AjaxResponse.success();
    }

    @ApiOperation(value = "更新材料")
    @ApiImplicitParams({
            @ApiImplicitParam(name="id",value="材料id"),
            @ApiImplicitParam(name="name",value="材料名"),
            @ApiImplicitParam(name="content",value="材料描述"),
            @ApiImplicitParam(name="filename",value="材料文件")
    })
    @PutMapping("/materials")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public AjaxResponse updateMaterial(@NotNull(message = "未传入材料id")@Min(value = 1,message = "材料id必须是正整数")@RequestParam("id") Integer id,
                                       @Size(min = 1,max = 50)@RequestParam(value = "name",required = false) String name,
                                       @Size(min = 1,max = 100)@RequestParam(value = "content",required = false) String content,
                                       @RequestParam(value = "filename",required = false) MultipartFile file) {
        //权限检测用户身份是管理员且登录
        //id为空或者id不存在
        if (!materialService.findMaterial(id).isPresent()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "材料id无效或未输入材料证书id!!!!");
        } else {
            Material material = materialService.findMaterial(id).get();
            if(file != null){
                String path = material.getPath();
                String newPath = FileUtil.FileCover(path,file);
                material.setPath(newPath);
            }
            if(name != null){
                material.setName(name);
            }
            if(content != null){
                material.setContent(content);
            }
            materialService.updateMaterial(material);
            return AjaxResponse.success();
        }
    }

    @ApiOperation(value = "删除材料")
    @ApiImplicitParams({
            @ApiImplicitParam(name="id",value="材料id")
    })
    @DeleteMapping("/materials/id={id}")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public AjaxResponse deleteMaterial(@NotNull(message = "id不能为空")@Min(value = 1,message = "材料id必须是正整数")@PathVariable Integer id) {
        //权限检测TODO 用户身份是管理员且登录
        //id为空或者id不存在
        if (!materialService.findMaterial(id).isPresent()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "材料id无效或未输入材料证书id!!!!");
        } else {
            FileUtil.deleteFile(materialService.findMaterial(id).get().getPath());
            materialService.deleteMaterial(id);
            return AjaxResponse.success();
        }
    }

    @ApiOperation(value = "得到单个材料")
    @ApiImplicitParams({
            @ApiImplicitParam(name="id",value="材料id")
    })
    @GetMapping("/materials/id={id}")
    public AjaxResponse findMaterial(@NotNull(message = "id不能为空")@Min(value = 1,message = "材料id必须是正整数")@PathVariable Integer id) {
        if (materialService.findMaterial(id).isPresent()) {
            Material material = materialService.findMaterial(id).get();
            return AjaxResponse.success(DozerUtil.map(material, MaterialVoGet.class));
        } else {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "材料id无效或未输入材料证书id!!!!");
        }
    }

    @ApiOperation(value = "得到材料列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name="page",value="页数"),
            @ApiImplicitParam(name="size",value="大小")
    })
    @GetMapping("/materials")
    public AjaxResponse findMaterials(@NotNull(message = "未传入页数")@Min(value = 1,message = "页数必须是正整数")@RequestParam("page") Integer page,
                                      @NotNull(message = "未传入每页的大小")@Min(value = 1,message = "每页的大小必须是正整数")@RequestParam("size") Integer size) {
        PageRequest pageRequest = PageRequest.of(page-1, size, Sort.Direction.DESC, "createTime");
        Page<Material> materialPage = materialService.findMaterials(pageRequest);
        List<Material> materialList = materialPage.getContent();
        List<MaterialVoGet> materialVoGets = DozerUtil.mapList(materialList, MaterialVoGet.class);
        PageData pageData = new PageData(materialPage.getTotalPages(), (int) materialPage.getTotalElements(),page, materialVoGets.size(), materialVoGets);
        return AjaxResponse.success(pageData);
    }
}
