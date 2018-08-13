%undefine _missing_build_ids_terminate_build
%define _libdir /usr/lib/erlang
%define _bindir /usr/bin

Summary: %{_summary}
Name: %{_name}
Version: %{_vers}
Release: %{_release}
Source: %{name}_%{version}.tar.gz
License: %{_license}
Group: "Development/Languages"
BuildArch: x86_64
BuildRoot: %{_topdir}/BUILDROOT/%{name}-%{version}.x86_64
BuildRequires:	ncurses-devel
BuildRequires:	openssl-devel
BuildRequires:	zlib-devel
BuildRequires:	flex
BuildRequires:	m4
BuildRequires:	fop
BuildRequires:	libxslt
BuildRequires:	emacs
BuildRequires:	emacs-el
BuildRequires:	ed
BuildRequires:	tcl-devel
BuildRequires:	tk-devel
BuildRequires:	java-devel
BuildRequires:	unixODBC-devel
BuildRequires:	wxGTK-devel
BuildRequires:	freeglut-devel
BuildRequires:	fop
BuildRequires:	libxslt-devel
BuildRequires:	chrpath

Vendor: %{_vendor}

%description
%_description

%post
echo "Erlang OTP %{version} installed"

%files
%defattr(-,root,root,-)
%{_libdir}/*
%{_bindir}/*
