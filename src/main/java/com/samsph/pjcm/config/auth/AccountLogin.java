package com.samsph.pjcm.config.auth;

import lombok.Data;

@Data
public class AccountLogin {
    private String username;
    private String password;
    private int role;
}
