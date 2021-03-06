package com.samsph.pjcm.web.controller;

import com.samsph.pjcm.config.PageData;
import com.samsph.pjcm.config.auth.CurrentUser;
import com.samsph.pjcm.config.constant.RoleType;
import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.model.User;
import com.samsph.pjcm.model.UserRole;
import com.samsph.pjcm.service.MailService;
import com.samsph.pjcm.service.UserRoleService;
import com.samsph.pjcm.service.UserService;
import com.samsph.pjcm.config.utils.DozerUtil;
import com.samsph.pjcm.config.utils.Sha256Util;
import com.samsph.pjcm.config.utils.UUIDUtil;
import com.samsph.pjcm.vo.*;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.parameters.P;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.*;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

@Api(tags = "用户管理")
@RestController
@Validated
@RequestMapping(value = "/api/v1")
public class UserController {
    @Autowired
    private UserService userService;
    @Autowired
    private UserRoleService userRoleService;
    @Autowired
    private MailService mailService;
    @Autowired
    private CurrentUser currentUser;

    @ApiOperation(value = "添加用户")
    @PostMapping("/users")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public AjaxResponse addUser(@Valid @RequestBody UserERVoPost userERVoPost){
        Integer role = userERVoPost.getRoleId();
        if (role == RoleType.CONTRIBUTOR_ROLE) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"不能添加作者只能注册添加");
        }
        if(userService.findUserByEmail(userERVoPost.getEmail()).isPresent()){
            User user1 = userService.findUserByEmail(userERVoPost.getEmail()).get();
            if(user1.getActive() != 1){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"账户待激活或已注销无法添加!!!");
            }else{
                if(userRoleService.findUserHasRole(user1.getId(),role)) {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "该邮箱已被注册!!!");
                }else{
                    UserRole userRole = new UserRole();
                    userRole.setUid(user1.getId());
                    userRole.setRole(role);
                    userRoleService.addUserRole(userRole);
                    return AjaxResponse.success();
                }
            }
        }else {
            userService.addUser(userERVoPost);
            Integer id = userService.findUserByEmail(userERVoPost.getEmail()).get().getId();
            UserRole userRole = new UserRole();
            userRole.setUid(id);
            userRole.setRole(role);
            userRoleService.addUserRole(userRole);
            return AjaxResponse.success();
        }
    }

    @ApiOperation(value = "用户激活")
    @ApiImplicitParams({
            @ApiImplicitParam(name="code",value="激活码")
    })
    @GetMapping("/users/checkCode")
    public AjaxResponse activeUser(@RequestParam("code") String code){
        if(userService.findUserByCode(code).isPresent()){
            User user = userService.findUserByCode(code).get();
            user.setActive(1);
            user.setCode("");
            userService.updateUser(user);
            return AjaxResponse.success();
        }else{
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"code错误!!!");
        }
    }

    @ApiOperation(value = "检查邮箱是否已被注册——只为注册提供")
    @ApiImplicitParams({
            @ApiImplicitParam(name="email",value="邮箱")
    })
    @GetMapping("/users/checkEmail")
    public AjaxResponse checkEmail(@RequestParam("email") String email){
        Map<String, Object> map = new HashMap<>();
        if(userService.findUserByEmail(email).isPresent()){
            User user = userService.findUserByEmail(email).get();
            if(userRoleService.findUserHasRole(user.getId(),RoleType.CONTRIBUTOR_ROLE)){
                map.put("result",1);
                map.put("reason","邮箱存在");
                return AjaxResponse.success(map);
            }
        }
        map.put("result",0);
        map.put("reason","邮箱不存在");
        return AjaxResponse.success(map);
    }

    @ApiOperation(value = "用户修改密码成功修改后返回首页")
    @ApiImplicitParams({
            @ApiImplicitParam(name="oldPassword",value="原密码"),
            @ApiImplicitParam(name="newPassword",value="新密码"),
            @ApiImplicitParam(name="id",value="用户id")
    })
    @PostMapping("/users/changePassword")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN','ROLE_EDITOR','ROLE_REVIEWER','ROLE_CONTRIBUTOR')")
    public AjaxResponse changePassword( @Size(min = 8,max = 20)@RequestParam("oldPassword") String oldPassword,
                                        @Size(min = 8,max = 20)@RequestParam("newPassword") String newPassword,
                                        @NotNull(message = "未传入用户id")@Min(value = 1,message = "用户id必须是正整数") @RequestParam("id") Integer id){
        //用户登录检测
//        if(currentUser.getCurrentUser().getUserId() != id){
//            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"无权修改其他人的密码");
//        }
        if(!userService.findUserByUid(id).isPresent()){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户不存在");
        }else{
            User user = userService.findUserByUid(id).get();
            if(user.getActive() != 1){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户未激活");
            }else {
                if (user.getPasswordHash().equals(Sha256Util.getSHA256StrJava(oldPassword))) {
                    user.setPasswordHash(Sha256Util.getSHA256StrJava(newPassword));
                    userService.updateUser(user);
                    return AjaxResponse.success("redirect:/");
                } else {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "原密码错误");
                }
            }
        }
    }

    @ApiOperation(value = "用户修改邮箱成功修改后返回首页")
    @ApiImplicitParams({
            @ApiImplicitParam(name="newEmail",value="新邮箱"),
            @ApiImplicitParam(name="id",value="用户id")
    })
    @PostMapping("/users/changeEmail")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN','ROLE_EDITOR','ROLE_REVIEWER','ROLE_CONTRIBUTOR')")
    public AjaxResponse changeEmail( @Email @RequestParam("newEmail") String newEmail,
                                     @NotNull(message = "未传入用户id")@Min(value = 1,message = "用户id必须是正整数") @RequestParam("id") Integer id){
        //用户登录检测
//        if(currentUser.getCurrentUser().getUserId() != id){
//            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"无权修改其他人的邮箱");
//        }
        if(!userService.findUserByUid(id).isPresent()){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户不存在");
        }else{
            User user = userService.findUserByUid(id).get();
            if(user.getActive() != 1){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户未激活");
            }else {
                    if(userService.findUserByEmail(newEmail).isPresent()){
                        throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"邮箱已注册");
                    }else{
                        String code = UUIDUtil.getUUID()+ UUIDUtil.getUUID();
                        user.setCode(code);
                        user.setActive(0);
                        user.setEmail(newEmail);
                        userService.updateUser(user);
                        mailService.sendHtmlMailForActive(newEmail,code);
                        //返回首页
                        return AjaxResponse.success("redirect:/");
                    }
            }
        }
    }

    @ApiOperation(value = "重置密码")
    @ApiImplicitParams({
            @ApiImplicitParam(name="id",value="用户id"),
            @ApiImplicitParam(name="newPassword",value="新密码")
    })
    @PostMapping("/users/resetPassword")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public AjaxResponse resetPassword( @Size(min = 8,max = 20)@RequestParam("newPassword") String newPassword,
                                       @NotNull(message = "未传入用户id")@Min(value = 1,message = "用户id必须是正整数") @RequestParam("id") Integer id){
        //管理员登录检测
        if(!userService.findUserByUid(id).isPresent()){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户不存在");
        }else{
            User user = userService.findUserByUid(id).get();
            if(user.getActive() != 1){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户未激活");
            }else {
                user.setPasswordHash(Sha256Util.getSHA256StrJava(newPassword));
                userService.updateUser(user);
                return AjaxResponse.success();
            }
        }
    }

    @ApiOperation(value = "重置邮箱")
    @ApiImplicitParams({
            @ApiImplicitParam(name="id",value="用户id"),
            @ApiImplicitParam(name="email",value="新邮箱")
    })
    @PostMapping("/users/resetEmail")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public AjaxResponse resetEmail(@NotNull(message = "未传入用户id")@Min(value = 1,message = "用户id必须是正整数") @RequestParam("id") Integer id,
                                   @Email @RequestParam("email") String email){
        //管理员登录检测
        if(!userService.findUserByUid(id).isPresent()){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户不存在");
        }else{
            User user = userService.findUserByUid(id).get();
            if(user.getActive() != 1){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户未激活");
            }else {
                if (!userService.findUserByEmail(email).isPresent()) {
                    user.setCode(UUIDUtil.getUUID() + UUIDUtil.getUUID());
                    user.setActive(0);
                    user.setEmail(email);
                    userService.updateUser(user);
                    mailService.sendHtmlMailForActive(email,user.getCode());
                    return AjaxResponse.success();
                } else {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "邮箱已注册");
                }
            }
        }
    }

    @ApiOperation(value = "修改用户信息")
    @PutMapping("/users")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN','ROLE_EDITOR','ROLE_REVIEWER','ROLE_CONTRIBUTOR')")
    public AjaxResponse updateUser(@Valid @RequestBody UserVoPut userVoPut){
        //用户登录检测
//        if(currentUser.getCurrentUser().getUserId() != userVoPut.getId()){
//            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"无权修改其他人的信息");
//        }
        if(!userService.findUserByUid(userVoPut.getId()).isPresent()){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户不存在");
        }else{
            User user = userService.findUserByUid(userVoPut.getId()).get();
            if(user.getActive() != 1){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户未激活");
            }else {
                if (userVoPut.getAddress() != null) {
                    user.setAddress(userVoPut.getAddress());
                }
                if (userVoPut.getBankAccount() != null) {
                    user.setBankAccount(userVoPut.getBankAccount());
                }
                if (userVoPut.getBankName() != null) {
                    user.setBankName(userVoPut.getBankName());
                }
                if (userVoPut.getEducation() != null) {
                    user.setEducation(userVoPut.getEducation());
                }
                if (userVoPut.getEmployer() != null) {
                    user.setEmployer(userVoPut.getEmployer());
                }
                if (userVoPut.getMajor() != null) {
                    user.setMajor(userVoPut.getMajor());
                }
                if (userVoPut.getPhone() != null) {
                    user.setPhone(userVoPut.getPhone());
                }
                if (userVoPut.getSex() != null) {
                    user.setSex(userVoPut.getSex());
                }
                if (userVoPut.getTitle() != null) {
                    user.setTitle(userVoPut.getTitle());
                }
                if (userVoPut.getUserName() != null) {
                    user.setUserName(userVoPut.getUserName());
                }
                if (userVoPut.getZipCode() != null) {
                    user.setZipCode(userVoPut.getZipCode());
                }
                userService.updateUser(user);
                return AjaxResponse.success();
            }
        }
    }

    @ApiOperation(value = "注销用户")
    @ApiImplicitParams({
            @ApiImplicitParam(name="uid",value="用户id"),
    })
    @DeleteMapping("/users/uid={uid}")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public AjaxResponse deleteUser(@NotNull(message = "uid不能为空")@Min(value = 1,message = "uid必须是正整数") @PathVariable Integer uid){
        //管理员检测
        if(userService.findUserByUid(uid).isPresent()){
            if(userService.findUserByUid(uid).get().getActive() != 1){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户未激活或已注销");
            }else {
                userService.deleteUser(uid);
                return AjaxResponse.success();
            }
        }else{
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"用户id输入错误!!!");
        }
    }

    @ApiOperation(value = "重新激活用户(对于已注销的用户)")
    @ApiImplicitParams({
            @ApiImplicitParam(name="uid",value="用户id"),
    })
    @PutMapping("/users/uid={uid}")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public AjaxResponse ActiveUser(@NotNull(message = "uid不能为空")@Min(value = 1,message = "uid必须是正整数") @PathVariable Integer uid){
        //管理员检测
        if(userService.findUserByUid(uid).isPresent()){
            if(userService.findUserByUid(uid).get().getActive() != -1){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户未注销");
            }else {
                User user = userService.findUserByUid(uid).get();
                user.setActive(1);
                userService.updateUser(user);
                return AjaxResponse.success();
            }
        }else{
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"用户id输入错误!!!");
        }
    }

    @ApiOperation(value = "得到用户个人信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name="uid",value="用户id"),
            @ApiImplicitParam(name="rid",value="角色id")
    })
    @GetMapping("/users/uid={uid}&rid={rid}")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN','ROLE_EDITOR','ROLE_REVIEWER','ROLE_CONTRIBUTOR')")
    public AjaxResponse findUserByUidAndRid(@NotNull(message = "uid不能为空")@Min(value = 1,message = "uid必须是正整数") @PathVariable Integer uid,
                                            @NotNull(message = "rid不能为空")@Min(value = RoleType.ADMIN_ROLE)@Max(value = RoleType.CONTRIBUTOR_ROLE) @PathVariable Integer rid){
//        if(currentUser.getCurrentUser().getUserRole() != RoleType.ADMIN_ROLE && currentUser.getCurrentUser().getUserRole() != RoleType.EDITOR_ROLE) {
//            if (currentUser.getCurrentUser().getUserId() != uid || currentUser.getCurrentUser().getUserRole() != rid) {
//                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "无权查看其他人的信息");
//            }
//        }
        Optional<User> userOptional = userService.findUserByUid(uid);
        if(userOptional.isPresent()) {
            if(!userRoleService.findUserHasRole(uid,rid)){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户没有此角色或者角色未激活或者角色已注销!!!");
            }else {
                switch (rid) {
                    case RoleType.ADMIN_ROLE:
                        UserAdminVoGet userAdminVoGet = DozerUtil.map(userOptional.get(), UserAdminVoGet.class);
                        return AjaxResponse.success(userAdminVoGet);
                    case RoleType.EDITOR_ROLE:
                        UserEditorVoGet userEditorVoGets = DozerUtil.map(userOptional.get(), UserEditorVoGet.class);
                        return AjaxResponse.success(userEditorVoGets);
                    case RoleType.REVIEWER_ROLE:
                        UserReviewerVoGet userReviewerVoGet = DozerUtil.map(userOptional.get(), UserReviewerVoGet.class);
                        return AjaxResponse.success(userReviewerVoGet);
                    case RoleType.CONTRIBUTOR_ROLE:
                        UserContributorVoGet userContributorVoGet = DozerUtil.map(userOptional.get(), UserContributorVoGet.class);
                        return AjaxResponse.success(userContributorVoGet);
                    default:
                        throw new CustomException(CustomExceptionType.OTHER_ERROR, "系统出现未知错误");
                }
            }
        }else{
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"uid无效或uid输入错误!!");
        }
    }

    @ApiOperation(value = "得到某个角色的所有用户")
    @ApiImplicitParams({
            @ApiImplicitParam(name="rid",value="角色id"),
            @ApiImplicitParam(name="page",value="页数"),
            @ApiImplicitParam(name="size",value="大小")
    })
    @GetMapping("/users")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public AjaxResponse findUsersByRoleId(@NotNull(message = "id不能为空")@Min(value = RoleType.ADMIN_ROLE)@Max(value = RoleType.CONTRIBUTOR_ROLE) @RequestParam("rid") Integer rid,
                                           @NotNull(message = "未传入页数")@Min(value = 1,message = "页数必须是正整数") @RequestParam("page") Integer page,
                                           @NotNull(message = "未传入每页的大小")@Min(value = 1,message = "每页的大小必须是正整数") @RequestParam("size") Integer size){
        PageRequest pageRequest = PageRequest.of(page-1,size);
        Page<User> userPage = userService.findUsersByRid(rid,pageRequest);
        List<User> userList = userPage.getContent();
        PageData pageData;
        if(userList != null){
            switch (rid){
                case RoleType.ADMIN_ROLE:
                    List<UserAdminVoGet> userAdminVoGets = DozerUtil.mapList(userList, UserAdminVoGet.class);
                    return AjaxResponse.success(userAdminVoGets);
                case RoleType.EDITOR_ROLE:
                    List<UserEditorVoGet> userEditorVoGets = DozerUtil.mapList(userList,UserEditorVoGet.class);
                    pageData = new PageData(userPage.getTotalPages(), (int) userPage.getTotalElements(),page,userEditorVoGets.size(),userEditorVoGets);
                    return AjaxResponse.success(pageData);
                case RoleType.REVIEWER_ROLE:
                    List<UserReviewerVoGet> userReviewerVoGets = DozerUtil.mapList(userList,UserReviewerVoGet.class);
                    pageData = new PageData(userPage.getTotalPages(), (int) userPage.getTotalElements(),page,userReviewerVoGets.size(),userReviewerVoGets);
                    return AjaxResponse.success(pageData);
                case RoleType.CONTRIBUTOR_ROLE:
                    List<UserContributorVoGet> userContributorVoGets = DozerUtil.mapList(userList,UserContributorVoGet.class);
                    pageData = new PageData(userPage.getTotalPages(), (int) userPage.getTotalElements(),page,userContributorVoGets.size(),userContributorVoGets);
                    return AjaxResponse.success(pageData);
                default:
                    throw new CustomException(CustomExceptionType.OTHER_ERROR,"系统出现未知错误");
            }
        }else {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"系统无对应角色的用户!!!");
        }
    }
}
