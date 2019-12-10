package com.samsph.pjcm.service.impl;

import com.samsph.pjcm.config.constant.RoleType;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.dao.UserRepository;
import com.samsph.pjcm.model.User;
import com.samsph.pjcm.model.UserRole;
import com.samsph.pjcm.service.MailService;
import com.samsph.pjcm.service.UserRoleService;
import com.samsph.pjcm.service.UserService;
import com.samsph.pjcm.config.utils.Sha256Util;
import com.samsph.pjcm.config.utils.UUIDUtil;
import com.samsph.pjcm.vo.UserVoPost;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Optional;

@Service
@Transactional
public class UserServiceImpl implements UserService {
    @Autowired
    private UserRepository userRepository;
    @Autowired
    private UserRoleService userRoleService;
    @Autowired
    private MailService mailService;

    @Override
    public void addUser(UserVoPost userVoPost) {
        java.sql.Date time = new java.sql.Date(new java.util.Date().getTime());
        String code = UUIDUtil.getUUID()+ UUIDUtil.getUUID();
        User user = new User();
        user.setActive(0);
        user.setCreateTime(time);
        user.setEmail(userVoPost.getEmail());
        user.setPasswordHash(Sha256Util.getSHA256StrJava(userVoPost.getPassword()));
        user.setCode(code);
        userRepository.save(user);
        mailService.sendHtmlMailForActive(user.getEmail(),code);
    }

    @Override
    public void updateUser(User user) {
        userRepository.save(user);
    }

    @Override
    public void deleteUser(int id) {
        //用户注销
        if(findUserByUid(id).isPresent()) {
            List<UserRole> userRoles = userRoleService.findUserRolesByUid(id);
            if(userRoles != null && userRoles.size()>0) {
                for (UserRole a : userRoles) {
                    userRoleService.deleteUserRole(a.getId());
                }
            }
            User user = findUserByUid(id).get();
            user.setActive(-1);
            updateUser(user);
        }else{
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"用户id不存在!!!");
        }
    }

    @Override
    public Optional<User> findUserByUid(int uid) {
        return userRepository.findById(uid);
    }

    @Override
    public Optional<User> findUserByEmail(String email) {
        return userRepository.findByEmail(email);
    }

    @Override
    public Page<User> findUsersByRid(int rid, Pageable pageable) {
        List<UserRole> list = userRoleService.findUserRolesByRole(rid);
        if(list!=null && list.size() > 0){
            List<Integer> editorList = new ArrayList<>();
            for(UserRole i : list){
                editorList.add(i.getUid());
            }
            Specification<User> specification = new Specification<User>() {
                @Override
                public Predicate toPredicate(Root<User> root, CriteriaQuery<?> criteriaQuery, CriteriaBuilder criteriaBuilder) {
                    List<Predicate> list = new ArrayList<>();
                    CriteriaBuilder.In<Object> in = criteriaBuilder.in(root.get("id"));
                    for (Integer id : editorList) {
                        in.value(id);
                    }
                    list.add(in);
                    Predicate[] p = new Predicate[list.size()];
                    return criteriaBuilder.and(list.toArray(p));
                }
            };
            return userRepository.findAll(specification, pageable);
        }else{
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"系统无对应角色的用户!!!");
        }
    }

    @Override
    public Optional<User> findUserByCode(String code) {
        return userRepository.findByCode(code);
    }

    @Override
    public void saveUser(UserVoPost userVoPost) {
        if(userVoPost.getRoleId()!= RoleType.CONTRIBUTOR_ROLE){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"只能注册作者用户!!!");
        }
        Optional<User> optionalUser = userRepository.findByEmail(userVoPost.getEmail());
        if (optionalUser.isPresent()) {
            if(optionalUser.get().getActive() != 1){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"账户待激活或已注销无法添加!!!");
            }else {
                if (userRoleService.findUserHasRole(optionalUser.get().getId(), RoleType.CONTRIBUTOR_ROLE)) {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "用户已经被注册!!!");
                } else {
                    UserRole userRole = new UserRole();
                    userRole.setRole(RoleType.CONTRIBUTOR_ROLE);
                    userRole.setUid(optionalUser.get().getId());
                    userRoleService.addUserRole(userRole);
                }
            }
        }else{
            User user = new User();
            String code = UUIDUtil.getUUID()+ UUIDUtil.getUUID();
            user.setEmail(userVoPost.getEmail());
            user.setPasswordHash(Sha256Util.getSHA256StrJava(userVoPost.getPassword()));
            user.setActive(0);
            user.setCreateTime(new Date(new java.util.Date().getTime()));
            user.setAddress(userVoPost.getAddress());
            user.setBankAccount(userVoPost.getBankAccount());
            user.setBankName(userVoPost.getBankName());
            user.setEducation(userVoPost.getEducation());
            user.setEmployer(userVoPost.getEmployer());
            user.setMajor(userVoPost.getMajor());
            user.setPhone(userVoPost.getPhone());
            user.setSex(userVoPost.getSex());
            user.setTitle(userVoPost.getTitle());
            user.setUserName(userVoPost.getUserName());
            user.setZipCode(userVoPost.getZipCode());
            user.setCode(code);
            userRepository.save(user);
            UserRole userRole = new UserRole();
            userRole.setRole(RoleType.CONTRIBUTOR_ROLE);
            userRole.setUid(userRepository.findByEmail(user.getEmail()).get().getId());
            userRoleService.addUserRole(userRole);
            mailService.sendHtmlMailForActive(user.getEmail(),code);
        }
    }
}
