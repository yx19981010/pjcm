package com.samsph.pjcm.service;

import com.samsph.pjcm.model.User;
import com.samsph.pjcm.vo.UserVoPost;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;


import java.util.Optional;

public interface UserService {
    public void addUser(UserVoPost userVoPost);

    public void updateUser(User user);

    public void deleteUser(int id);

    public Optional<User> findUserByUid(int uid);

    public Optional<User> findUserByEmail(String email);

    public Page<User> findUsersByRid(int rid, Pageable pageable);

    public Optional<User> findUserByCode(String code);

    public void saveUser(UserVoPost userVoPost);

}
