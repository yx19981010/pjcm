package com.samsph.pjcm.service;

import com.samsph.pjcm.model.UserRole;
import org.springframework.security.core.authority.SimpleGrantedAuthority;


import java.util.List;

public interface UserRoleService {
    public void addUserRole(UserRole userRole);

    public void updateUserRole(UserRole userRole);

    public void deleteUserRole(int id);

    public UserRole findUserRole(int id);

    public List<UserRole> findUserRolesByUid(int uid);

    public List<UserRole> findUserRolesByRole(int role);

    public boolean findUserHasRole(int uid,int role);

    public List<SimpleGrantedAuthority> getRoles(Integer uid);
}
