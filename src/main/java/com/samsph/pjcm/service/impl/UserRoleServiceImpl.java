package com.samsph.pjcm.service.impl;

import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.dao.UserRepository;
import com.samsph.pjcm.dao.UserRoleRepository;
import com.samsph.pjcm.model.User;
import com.samsph.pjcm.model.UserRole;
import com.samsph.pjcm.service.UserRoleService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

@Service
@Transactional
public class UserRoleServiceImpl implements UserRoleService {
    @Autowired
    private UserRoleRepository userRoleRepository;
    @Autowired
    private UserRepository userRepository;
    @Override
    public void addUserRole(UserRole userRole) {
        userRoleRepository.save(userRole);
    }

    @Override
    public void updateUserRole(UserRole userRole) {
        userRoleRepository.save(userRole);
    }

    @Override
    public void deleteUserRole(int id) {
        userRoleRepository.deleteById(id);
    }

    @Override
    public UserRole findUserRole(int id) {
        if(userRoleRepository.findById(id).isPresent()){
            UserRole userRole = userRoleRepository.findById(id).get();
            if(userRepository.findById(userRole.getUid()).get().getActive() == 1){
                return userRole;
            }else {
                return null;
            }
        }else{
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"用户角色表无此ID对应的表项!!!");
        }
    }

    @Override
    public List<UserRole> findUserRolesByUid(int uid) {
        if(userRepository.findById(uid).isPresent()) {
            User user = userRepository.findById(uid).get();
            if (user.getActive() == 1) {
                return userRoleRepository.findByUid(uid);
            }
            return null;
        }else {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"用户ID输入错误!!!");
        }
    }

    @Override
    public List<UserRole> findUserRolesByRole(int role) {
        List<UserRole> userRoles = userRoleRepository.findByRole(role);
        List<UserRole> userRoleList = new ArrayList<>();
        if(userRoles != null && userRoles.size()>0){
            for(UserRole a : userRoles){
                if(userRepository.findById(a.getUid()).isPresent()){
                    if(userRepository.findById(a.getUid()).get().getActive() == 1){
                        userRoleList.add(a);
                    }
                }
            }
        }
        return userRoleList;
    }

    @Override
    public boolean findUserHasRole(int uid, int role) {
        if(userRepository.findById(uid).isPresent()){
            User user = userRepository.findById(uid).get();
            if(user.getActive() == 1){
                return userRoleRepository.findUserHasRole(uid,role).isPresent();
            }
        }
        return false;
    }
}
